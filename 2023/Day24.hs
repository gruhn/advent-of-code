module Main where

import Utils (Parser, parseFile, Vec3(..), symbol, integer, toVec3, Vec2(..), assertM)
import Text.Megaparsec (sepEndBy, sepBy)
import Text.Megaparsec.Char (newline)
import Data.List (tails)
import Control.Monad (guard)
import Data.Maybe (maybeToList)
import qualified Data.IntMap as IntMap
import Theory.NonLinearRealArithmatic.Expr (Expr(..), Var)
import Theory.NonLinearRealArithmatic.Constraint (Constraint, ConstraintRelation (..))
import qualified Theory.NonLinearRealArithmatic.Polynomial as Polynomial
import Theory.NonLinearRealArithmatic.BoundedFloating (BoundedFloating (..))
import Theory.NonLinearRealArithmatic.Polynomial (Assignment)
import Theory.NonLinearRealArithmatic.IntervalUnion (IntervalUnion(..))
import qualified Theory.NonLinearRealArithmatic.IntervalUnion as IntervalUnion
import Theory.NonLinearRealArithmatic.Interval (Interval(..))
import Theory.NonLinearRealArithmatic.IntervalConstraintPropagation (intervalConstraintPropagation)
import Data.Foldable (minimumBy, traverse_)
import Data.Function (on)
import Theory.NonLinearRealArithmatic.IntervalUnion (diameter)

type Trajectory2 = (Vec2 Rational, Vec2 Rational)

type Trajectory3 a = (Vec3 a, Vec3 a)

parser :: Parser [Trajectory3 Rational]
parser = trajectory `sepEndBy` newline
  where
    rational :: Parser Rational
    rational = fromIntegral <$> integer

    vec :: Parser (Vec3 Rational)
    vec = toVec3 <$> rational `sepBy` symbol ","

    trajectory :: Parser (Trajectory3 Rational)
    trajectory = do 
      pos <- vec
      symbol "@"
      vel <- vec
      return (pos, vel)

intersectionPoint :: Trajectory2 -> Trajectory2 -> Maybe (Vec2 Rational)
intersectionPoint (Vec2 x1 y1, Vec2 vx1 vy1) (Vec2 x2 y2, Vec2 vx2 vy2) = do
  let denom = vy1*vx2 - vx1*vy2

  -- if denominator is zero, the trajectories are parallel:
  guard $ denom /= 0

  let time1 = (x1-x2)*vy2 / denom - (y1-y2)*vx2 / denom
  let time2 = (x1-x2)*vy1 / denom - (y1-y2)*vx1 / denom
  -- reject negative times, since they would lie in the past:
  guard $ time1 >= 0
  guard $ time2 >= 0

  let point1 = Vec2 x1 y1 + Vec2 (vx1*time1) (vy1*time1)
  let point2 = Vec2 x2 y2 + Vec2 (vx2*time2) (vy2*time2)
  assertM (point1 == point2)

  return point1

projectXY :: Trajectory3 Rational -> Trajectory2
projectXY (Vec3 x y _, Vec3 vx vy _) = (Vec2 x y, Vec2 vx vy)

{-

collisionConstraints :: [Trajectory3 Rational] -> Trajectory3 SReal -> ConstraintSet
collisionConstraints trajectories (Vec3 x y z, Vec3 vx vy vz) = 
  let
    go :: Int -> Trajectory3 Rational -> ConstraintSet
    go index (Vec3 x' y' z', Vec3 vx' vy' vz') = do
      time <- sReal ("t" ++ show index)
      constrain $ time .>= 0
      constrain $ x - time*vx .== fromRational x' + time * fromRational vx'
      constrain $ y - time*vy .== fromRational y' + time * fromRational vy'
      constrain $ z - time*vz .== fromRational z' + time * fromRational vz'
  in
    zipWithM_ go [1..] trajectories
-}

type BoundedRational = BoundedFloating Double

type VarDomain = IntervalUnion BoundedRational

collisionConstraints :: [Trajectory3 Rational] -> (Vec3 Var, Assignment VarDomain, [Constraint BoundedRational])
collisionConstraints trajectories = 
  let 
    sol_x, sol_y, sol_z, sol_vx, sol_vy, sol_vz :: Expr BoundedRational
    sol_x  = Var 1
    sol_y  = Var 2
    sol_z  = Var 3
    sol_vx = Var 4
    sol_vy = Var 5
    sol_vz = Var 6

    symbolic_solution :: Vec3 Var
    symbolic_solution = Vec3 1 2 3

    time_vars :: [Expr BoundedRational]
    time_vars = [ Var (i+7) | i <- [1 .. length trajectories] ]

    initial_domains :: Assignment VarDomain
    initial_domains = IntMap.fromList [ (var, IntervalUnion [ Val (-400000000000000) :..: Val 400000000000000 ]) | var <- [1 .. length trajectories + 6 ] ]

    constant :: Rational -> Expr BoundedRational
    constant = Const . Val . fromRational

    {-| 
      build constraints:
        0 <= time
        0 = (sol_x - x) + time * (sol_vx - vx)
        0 = (sol_y - y) + time * (sol_vy - vy)
        0 = (sol_z - z) + time * (sol_vz - vz)
    -}
    constraints_from :: Expr BoundedRational -> Trajectory3 Rational -> [Constraint BoundedRational]
    constraints_from time (Vec3 x y z, Vec3 vx vy vz) = 
      [ (LessEquals, Polynomial.fromExpr time)
      , (Equals, Polynomial.fromExpr $ sol_x - constant x + time * (sol_vx - constant vx))
      , (Equals, Polynomial.fromExpr $ sol_y - constant y + time * (sol_vy - constant vy))
      , (Equals, Polynomial.fromExpr $ sol_z - constant z + time * (sol_vz - constant vz))
      ]

    constraints :: [Constraint BoundedRational]
    constraints = concat $ zipWith constraints_from time_vars trajectories
  in
    (symbolic_solution, initial_domains, constraints)

toIntOrSplit :: IntervalUnion BoundedRational -> IntervalUnion BoundedRational 
toIntOrSplit = IntervalUnion.modifyIntervals (concatMap go)
  where
    go :: Interval BoundedRational -> [Interval BoundedRational]
    go interval@(Val lb :..: Val ub) 
      | ceiling lb == floor ub = [ Val (fromIntegral $ floor ub) :..: Val (fromIntegral $ floor ub) ]
      | ceiling lb >  floor ub = []
      | otherwise = split interval
    go internval = split internval

    split :: Interval BoundedRational -> [Interval BoundedRational]
    split (NegInf :..: PosInf)  = [ NegInf :..: Val 0, Val 0 :..: PosInf ]
    split (NegInf :..: Val val) = [ NegInf :..: Val (abs val * (-2)), Val (abs val * (-2)) :..: Val val ]
    split (Val val :..: PosInf) = [ Val val :..: Val (abs val * 2), Val (abs val * 2) :..: PosInf ]
    split (Val lb :..: Val ub) = [ Val lb :..: Val ((ub+lb)/2), Val ((ub+lb)/2) :..: Val ub ]
    split _ = undefined

main :: IO ()
main = do
  trajectories <- parseFile parser "input/24.txt"

  putStr "Part 1: "
  print $ length $ do
    traj1:rest <- tails $ map projectXY trajectories
    traj2      <- rest

    Vec2 x y <- maybeToList $ intersectionPoint traj1 traj2
    guard $ 200000000000000 <= x && x <= 400000000000000
    guard $ 200000000000000 <= y && y <= 400000000000000

    return (Vec2 x y)

  putStr "Part 2: "
  let (symbolic_solution, initial_domains, constraints) = collisionConstraints trajectories

  -- traverse_ print constraints

  traverse_ print 
    -- $ minimum
    -- $ map (diameter . snd)
    $ IntMap.toList
    $ (!! 1) 
    $ iterate (IntMap.map toIntOrSplit . intervalConstraintPropagation constraints) initial_domains
