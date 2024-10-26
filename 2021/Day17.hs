module Main (main) where

import Data.Foldable (maximumBy)
import Data.Function (on)
import Data.Ratio (Ratio, (%))
import Control.Monad (guard)
import Test.QuickCheck (Property, (===), (==>), counterexample, quickCheck, forAll, shrink, NonNegative (NonNegative), Testable (property), Negative (Negative))
import Test.QuickCheck.Gen (chooseInt)
import Test.QuickCheck.Property (forAllShrink)
import Test.QuickCheck.Modifiers (NonNegative)

type Config = (Int, Int, Int, Int)

targetArea :: (Int, Int, Int, Int)
targetArea = (70,125,-159,-121)
-- targetArea = (20,30,-10,-5)

step :: Config -> Config
step (x, y, vx, vy) = 
    (x + vx, y + vy, vx - signum vx, vy - 1)

pastTargetArea :: Config -> Bool
pastTargetArea (x,y,_,_) =
    let (_, xFar, yLow, _) = targetArea
    in xFar < x || y < yLow

inTargetArea :: Config -> Bool
inTargetArea (x,y,_,_) = 
    let (xClose, xFar, yLow, yHigh) = targetArea
    in and [ xClose <= x, x <= xFar, yLow <= y, y <= yHigh ]

trajectory :: Config -> [Config]
trajectory = takeWhile (not . pastTargetArea) . iterate step

maxHeight :: Config -> Int
maxHeight (_,y,_,vy) =
    y + (signum vy * vy^2 + vy) `div` 2

stepsTo :: Int -> Int -> Int
stepsTo xInt vxInt =
    let x = fromIntegral xInt
        vx = fromIntegral vxInt
    -- solve for n: x = vx + (vx-1) + (vx-2) + ... + (vx-n)
    in ceiling $ 1 + (2*vx -1)/2 - sqrt (vx^2 + vx + 1/2 - 2*x) 

intSqrt :: Int -> (Int, Int)
intSqrt n
  | n < 0     = undefined
  | otherwise = try 0
  where
    try :: Int -> (Int, Int)
    try i 
      | i*i >  n  = (i-1, i)
      | i*i == n  = (i, i)
      | otherwise = try (i+1)

main0 :: IO ()
main0 = do
    let (xClose, xFar, yLow, yHigh) = targetArea
        vxMin = ceiling $ sqrt (fromIntegral (xClose*2) + 1/4) - 1/2
        vxMax = xFar

        vyMin :: Int -> Int
        vyMin vx = 
            let n = fromIntegral $ stepsTo xClose vx 
            in floor $ fromIntegral yLow / n + (n-1)/2

        -- random guess
        vyMax vx = 1000 

        startConfigs =
            [ (0, 0, vx, vy)
            | vx <- [vxMin .. vxMax]
            , vy <- [vyMin vx .. vyMax vx] 
            ]

        hitConfigs 
            = map head
            . filter (inTargetArea . last)
            $ map trajectory startConfigs

    putStr "Part 1: "
    print $ maximum . map maxHeight $ hitConfigs

    putStr "Part 2: "
    print $ length hitConfigs

xPos :: Int -> Int -> Int
xPos vel time
  | abs vel < time = xPos vel (abs vel)
  | otherwise = time*vel - signum vel * (time^2 - time) `div` 2
  -- let
  --   time' = min (time-1) (abs vel)
  -- in
  --   time*vel
  --   - signum vel * ((time'^2 + time') `div` 2) 
  --   - vel * max 0 (time - abs vel - 1)


yPos :: Int -> Int -> Int
yPos velocity time = time*velocity - (time^2 - time) `div` 2

xVelRange :: (Int, Int) -> Int -> [Int]
xVelRange (target_x_min, target_x_max) time =
  let
    -- for: time <= vel
    range3 :: [Int]
    range3 = 
      let
        lower_bound :: Ratio Int
        lower_bound = (2*target_x_min + time^2 - time) % (2*time)

        upper_bound :: Ratio Int
        upper_bound = (2*target_x_max + time^2 - time) % (2*time)

        lub = min time (floor upper_bound)
      in
        [ceiling lower_bound .. lub]
  in
    range3

yVelRange :: (Int, Int) -> Int -> [Int]
yVelRange (target_y_min, target_y_max) time = 
  let
    lower_bound :: Ratio Int
    lower_bound = (2*target_y_min + time^2 - time) % (2*time)

    upper_bound :: Ratio Int
    upper_bound = (2*target_y_max + time^2 - time) % (2*time)
  in
    [ceiling lower_bound .. floor upper_bound]

main :: IO ()
main = do
  quickCheck prop_xPos
  quickCheck prop_yPos

  main0
 
-----------------------------------------------------------------
----------------------------- TESTS -----------------------------
-----------------------------------------------------------------

prop_intSqrt :: NonNegative Int -> Property
prop_intSqrt (NonNegative n) = property $ 
  let
    (root_lb, root_ub) = intSqrt n   
  in
    root_ub - root_lb <= 1 && root_lb^2 <= n && n <= root_ub^2

xTrajectory :: Int -> [Int]
xTrajectory initial_vel = 
  let
    step :: (Int, Int) -> (Int, Int)
    step (pos, vel) 
      | vel > 0   = (pos+vel, vel-1)
      | vel < 0   = (pos+vel, vel+1)
      | otherwise = (pos, 0)
  in
    map fst $ iterate step (0, initial_vel)

prop_xPos :: NonNegative Int -> Int -> Property
prop_xPos (NonNegative time) vel = 
  counterexample ("(vel,time) = " ++ show (vel, time)) $
  xPos vel time === xTrajectory vel !! time

yTrajectory :: Int -> [Int]
yTrajectory initial_vel = 
  let
    step :: (Int, Int) -> (Int, Int)
    step (pos, vel) = (pos+vel, vel-1)
  in
    map fst $ iterate step (0, initial_vel)

prop_yPos :: NonNegative Int -> Int -> Property
prop_yPos (NonNegative time) vel =  
  counterexample ("(vel,time) = " ++ show (vel, time)) $
  yPos vel time === yTrajectory vel !! time
