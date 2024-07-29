{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Main (main) where
import ParseUtils (Parser, symbol, integer, parseWith)
import Text.Megaparsec.Char (newline, string)
import Text.Megaparsec (sepEndBy)
import qualified Data.Set as Set

type Point = (Int, Int)

data Trajectory = Traj 
  { position :: Point 
  , velocity :: Point 
  } deriving Show

parser :: Parser [Trajectory]
parser = trajectory `sepEndBy` newline
  where
    point :: Parser Point
    point = do
      symbol "<"
      x <- integer
      symbol ","
      y <- integer
      symbol ">"
      return (x,y)

    trajectory :: Parser Trajectory
    trajectory = do
      string "position="
      pos <- point
      string "velocity="
      vel <- point
      return $ Traj pos vel

showGrid :: [Point] -> String
showGrid points =
  let
    xs = map fst points
    ys = map snd points

    min_x = minimum xs - 1
    max_x = maximum xs + 1
    min_y = minimum ys - 1
    max_y = maximum ys + 1

    point_set = Set.fromList points

    show_line :: Int -> String
    show_line y = do
      x <- [min_x .. max_x]
      if Set.member (x,y) point_set then
        return '#'
      else
        return '.'
  in 
    unlines $ map show_line [min_y .. max_y]

positionAt :: Int -> Trajectory -> Point
positionAt time (Traj (px, py) (vx, vy)) = 
  (px + vx*time, py + vy*time)

main :: IO ()
main = do
  trajectories <- parseWith parser "input/10.txt"

  -- let time = timeOfMinVariance' trajectories
  let time = timeOfMinVariance trajectories

  -- TEST:
  -- print $ timeOfMinVariance trajectories == timeOfMinVariance' trajectories

  putStrLn "Part 1: "
  putStr $ showGrid $ map (positionAt time) trajectories

  putStr "Part 2: "
  print time

---- ANALYTIC SOLUTION ----

-- | solve for t: d/dt Var(X) + Var(Y) = 0
timeOfMinVariance :: [Trajectory] -> Int
timeOfMinVariance trajs = 
  let
    n = length trajs 

    px_sum = sum [ px | Traj (px, _) _ <- trajs ]
    py_sum = sum [ py | Traj (_, py) _ <- trajs ]

    vx_sum = sum [ vx | Traj _ (vx, _) <- trajs ]
    vy_sum = sum [ vy | Traj _ (_, vy) <- trajs ]

    vx_vx_sum = sum [ vx^2 | Traj _ (vx, _) <- trajs ]
    vy_vy_sum = sum [ vy^2 | Traj _ (_, vy) <- trajs ]

    px_vx_sum = sum [ px*vx | Traj (px, _) (vx, _) <- trajs ]
    py_vy_sum = sum [ py*vy | Traj (_, py) (_, vy) <- trajs ]

    numerator_x = px_sum*vx_sum - n*px_vx_sum 
    numerator_y = py_sum*vy_sum - n*py_vy_sum
    denominator_x = n*vx_vx_sum - vx_sum^2 
    denominator_y = n*vy_vy_sum - vy_sum^2
  in
    (numerator_x + numerator_y) `div` (denominator_x + denominator_y)

---- BRUTE FORCE SOLUTION ----

timeOfMinVariance' :: [Trajectory] -> Int
timeOfMinVariance' = go (-1) maxBound
  where
    go :: Int -> Int -> [Trajectory] -> Int
    go time prev_variance trajs = 
      let 
        points = map position trajs
        curr_variance = variance points
      in
        if prev_variance < curr_variance then
          time
        else
          go (time+1) curr_variance (simulateStep trajs)
        
variance :: [Point] -> Int
variance points = 
  let 
    xs = map fst points
    ys = map snd points

    x_avg = sum xs `div` length xs
    y_avg = sum ys `div` length ys

    x_var = sum [ (x - x_avg)^2 | x <- xs ]
    y_var = sum [ (y - y_avg)^2 | y <- ys ]
  in
    x_var + y_var

simulateStep :: [Trajectory] -> [Trajectory]
simulateStep = map go
  where
    go :: Trajectory -> Trajectory
    go traj@(Traj (px, py) (vx, vy)) = 
      traj { position = (px + vx, py + vy) }
