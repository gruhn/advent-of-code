module Main where

import Utils (Parser, parseFile, Vec3(..), symbol, integer, toVec3, Vec2(..), assertM)
import Text.Megaparsec (sepEndBy, sepBy)
import Text.Megaparsec.Char (newline)
import Data.List (tails)
import Control.Monad (guard)
import Data.Maybe (maybeToList, fromJust)
import qualified Data.IntMap as IntMap
import Data.Foldable (traverse_)
import Theory.LinearArithmatic.Constraint (Constraint, Var, AffineExpr(..), (.<=), (.==))
import qualified Theory.LinearArithmatic.Simplex as Simplex
import Theory.LinearArithmatic.Simplex (BoundType(..))
import qualified Data.IntSet as IntSet

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

collisionConstraints :: [Trajectory3 Rational] -> (Vec3 Var, [Constraint])
collisionConstraints trajectories = 
  let 
    var_expr :: Var -> AffineExpr
    var_expr var = AffineExpr 0 (IntMap.singleton var 1)

    const_expr :: Rational -> AffineExpr
    const_expr c = AffineExpr c IntMap.empty

    sol_x, sol_y, sol_z, sol_vx, sol_vy, sol_vz :: AffineExpr
    sol_x  = var_expr 1
    sol_y  = var_expr 2
    sol_z  = var_expr 3
    sol_vx = var_expr 4
    sol_vy = var_expr 5
    sol_vz = var_expr 6

    symbolic_solution :: Vec3 Var
    symbolic_solution = Vec3 1 2 3

    {-| 
      build constraints:
        0 <= time
        sol_x + time*sol_vx = time*vx + x 
        0 = (sol_y - y) + time * (sol_vy - vy)
        0 = (sol_z - z) + time * (sol_vz - vz)
    -}
    constraints_from :: Trajectory3 Rational -> (Var, [Constraint]) -> (Var, [Constraint])
    constraints_from (Vec3 x y z, Vec3 vx vy vz) (max_var, constrs) = 
      let 
        time = var_expr (max_var+1)
        time_sol_vx = var_expr (max_var+2) 
        time_sol_vy = var_expr (max_var+3)
        time_sol_vz = var_expr (max_var+4)

        new_constrs = 
          [ 0 .<= time
          , (sol_x + time * const_expr vx) .== (const_expr x + time_sol_vx)
          , (sol_y + time * const_expr vy) .== (const_expr y + time_sol_vy)
          , (sol_z + time * const_expr vz) .== (const_expr z + time_sol_vz)
          ]
      in
        (max_var+5, new_constrs ++ constrs)
  in
    (symbolic_solution, snd $ foldr constraints_from (7, []) trajectories)

boundViolations :: Simplex.Tableau -> Int
boundViolations tableau = sum $ do
  (var, current_value) <- IntMap.toList $ Simplex.getAssignment tableau
  bound <- maybeToList $ IntMap.lookup var $ Simplex.getBounds tableau
  case bound of 
    (UpperBound, bound_value) 
      | current_value <= bound_value -> return 0
      | otherwise                    -> return 1
    (LowerBound, bound_value)
      | bound_value <= current_value -> return 0
      | otherwise                    -> return 1

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
  let (_, constraints) = collisionConstraints trajectories
      vars = IntSet.fromList [1..6]

  traverse_ print 
    -- $ take 100
    -- $ Simplex.isBoundViolated
    $ map (\t -> ( boundViolations t , IntMap.restrictKeys (Simplex.getAssignment t) vars))
    $ Simplex.steps 
    $ fromJust 
    $ Simplex.initTableau constraints
  -- traverse_ print $ Simplex.steps $ fromJust $ Simplex.initTableau constriants
