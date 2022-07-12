module Main where

import IntcodeComputer
import Text.Megaparsec (errorBundlePretty, parse)
import qualified Data.Map as Map
import Lens.Micro (set, (^.))

data Direction = North | East | South | West
    deriving (Eq, Show)

directions :: [Direction]
directions = [North, East, South, West]

turnRight :: Direction -> Direction
turnRight dir = 
    (!! 1) . dropWhile (/= dir) . cycle $ directions

-- >>> turnLeft North
-- West

turnLeft :: Direction -> Direction
turnLeft dir =
    (!! 1) . dropWhile (/= dir) . cycle . reverse $ directions

type Pos = (Integer,Integer)

move :: Direction -> Pos -> Pos
move North (x,y) = (x,y-1)
move East (x,y) = (x+1,y)
move South (x,y) = (x,y+1)
move West (x,y) = (x-1,y)

turn :: Integer -> Direction -> Direction
turn 0 = turnLeft
turn 1 = turnRight
turn _ = error "unknown turn code"

type Area = Map.Map Pos Integer

type PaintRobotState = (State, Pos, Direction, Area)

initialState' :: Program -> PaintRobotState
initialState' program =
    (initialState program [], (0,0), North, Map.empty)

step' :: PaintRobotState -> PaintRobotState
step' (state0, pos0, dir0, area0) =
    let color0 = Map.findWithDefault 0 pos0 area0
        state1 = depletInput (set input [color0] state0)
        (color1:turnCode:_) = state1 ^. output

        state3 = set output [] state1
        area1 = Map.insert pos1 color1 area0
        dir1 = turn turnCode dir0
        pos1 = move dir1 pos0

    in  if hasHalted state1 then
            (state0, pos0, dir0, area0)
        else
            (state3, pos1, dir1, area1)

main :: IO ()
main = do
    input <- parse parser "" <$> readFile "2019/input/11.txt"
    case input of
        Left error -> putStr (errorBundlePretty error)
        Right program -> do 
            let (_, _, _, area) = converge step' (initialState' program)
            print $ Map.toList area

-- 787 too low
