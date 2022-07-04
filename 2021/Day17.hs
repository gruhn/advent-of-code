module Main where
import Data.Foldable (maximumBy)
import Data.Function (on)

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
trajectory = 
    takeWhile (not . pastTargetArea) . iterate step

maxHeight :: Config -> Int
maxHeight (_,y,_,vy) =
    y + (signum vy * vy^2 + vy) `div` 2

stepsTo :: Int -> Int -> Int
stepsTo xInt vxInt =
    let x = fromIntegral xInt
        vx = fromIntegral vxInt
    -- solve for n: x = vx + (vx-1) + (vx-2) + ... + (vx-n)
    in ceiling $ 1 + (2*vx -1)/2 - sqrt (vx^2 + vx + 1/2 - 2*x) 

main :: IO ()
main = do
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