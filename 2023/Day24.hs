module Main where

import Utils (Parser, parseFile, Vec3(..), symbol, integer, toVec3, Vec2(..), assertM)
import Text.Megaparsec (sepEndBy, sepBy)
import Text.Megaparsec.Char (newline)
import Data.List (tails)
import Control.Monad (guard, zipWithM_)
import Data.Maybe (maybeToList)
import Data.SBV (ConstraintSet, (.>=), constrain, (.==), sat, Symbolic, sReal, SReal, sReals, Modelable (getModelValue), AlgReal)

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

collisionTrajectory :: Symbolic (Trajectory3 SReal)
collisionTrajectory = do
  [x, y, z, vx, vy, vz] <- sReals ["x", "y", "z", "vx", "vy", "vz"]
  return (Vec3 x y z, Vec3 vx vy vz)

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
  solution <- sat $ do
    traj <- collisionTrajectory
    collisionConstraints trajectories traj

  print $ do
    x <- getModelValue "x" solution
    y <- getModelValue "y" solution
    z <- getModelValue "z" solution
    return (x + y + z :: AlgReal)
