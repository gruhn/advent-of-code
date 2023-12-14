module Main where
import Utils (withCoords, converge, takeDistinct, fixpoint, findCycle)
import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad (guard)
import Data.Function (on)
import Data.Foldable (maximumBy)
import Data.List (findIndex, elemIndex, iterate')
import Data.Maybe (fromJust)
import Debug.Trace (traceShow)

type Pos = (Int,Int)

data Dish = Dish 
  { getRoundRocks  :: Set Pos
  , getSquareRocks :: Set Pos
  , getBoundX      :: Int
  , getBoundY      :: Int
  } deriving (Eq, Ord)

modifyRoundRocks :: (Set Pos -> Set Pos) -> Dish -> Dish
modifyRoundRocks f dish = dish { getRoundRocks = f (getRoundRocks dish) }

parse :: String -> Dish
parse input = Dish round_rocks square_rocks bound_x bound_y
  where
    poses = withCoords $ lines input
    (bound_x, bound_y) = maximum $ map fst poses

    round_rocks  = Set.fromList $ map fst $ filter ((=='O') . snd) poses
    square_rocks = Set.fromList $ map fst $ filter ((=='#') . snd) poses

isRoundRock :: Dish -> Pos -> Bool
isRoundRock dish pos = pos `Set.member` getRoundRocks dish

isSquareRock :: Dish -> Pos -> Bool
isSquareRock dish pos = pos `Set.member` getSquareRocks dish

isFree :: Dish -> Pos -> Bool
isFree dish pos = 
     not (isRoundRock dish pos) 
  && not (isSquareRock dish pos)

data Dir = North | East | South | West

roll :: Dish -> Dir -> Pos -> Pos
roll dish dir (x,y) = 
    last 
  $ ((x,y) :) 
  -- $ filter isFree 
  -- $ takeWhile (not . isSquareRock dish) 
  $ takeWhile (isFree dish)
  $ case dir of
      North -> map (x,) $ reverse [0 .. y-1]
      South -> map (x,) [y+1 .. getBoundY dish]
      West  -> map (,y) $ reverse [0 .. x-1]
      East  -> map (,y) [x+1 .. getBoundX dish]

-- dependencies :: Dir -> Pos -> Set Pos
-- dependencies dir (x,y) = 
--   Set.singleton $ case dir of
--     North -> (x,y+1)
--     South -> (x,y-1)
--     West  -> (x+1,y)
--     East  -> (x+1,y)

-- tilt :: Dir -> Dish -> Dish
-- tilt dir dish = update_dish final_mapping
--   where
--     update :: Map Pos Pos -> Pos -> Pos
--     update mapping = roll (update_dish mapping) dir

--     update_dish :: Map Pos Pos -> Dish
--     update_dish mapping = modifyRoundRocks (const $ Set.fromList $ Map.elems mapping) dish

--     initial_worklist = getRoundRocks dish
--     initial_mapping = Map.fromSet id (getRoundRocks dish)

--     final_mapping = fixpoint (dependencies dir) initial_worklist update initial_mapping

   -- in converge naive dish

tilt :: Dir -> Dish -> Dish
tilt dir dish = modifyRoundRocks (converge go) dish
  where 
    go :: Set Pos -> Set Pos
    go rocks = Set.map (roll (modifyRoundRocks (const rocks) dish) dir) rocks

spinCycle :: Dish -> Dish
spinCycle = tilt East . tilt South . tilt West . tilt North

score :: Dish -> Int
score (Dish round_rocks _ _ south_edge) = sum $ do
  (_, y) <- Set.toList round_rocks
  return (south_edge + 1 - y)

main :: IO ()
main = do
  dish <- parse <$> readFile "input/14.txt"

  putStr "Part 1: "
  print $ score $ tilt North dish

  putStr "Part 2: "
  print $ do
    (prefix, loop) <- findCycle 
        $ take (10^9)
        $ iterate spinCycle dish
    let rest = (10^9 - length prefix) `rem` length loop
    return $ score $ iterate spinCycle (head loop) !! rest


  -- print 
  --   $ (Set.\\ getRoundRocks (tilt west dish))
  --   $ getRoundRocks
  --   $ tilt west
  --   $ tilt east 
  --   $ tilt west dish

  -- print 
  --   $ (Set.\\ getRoundRocks (tilt east dish))
  --   $ getRoundRocks
  --   $ tilt east
  --   $ tilt west
  --   $ tilt east dish

  -- print $ fmap (length . fst) $ findCycle $ take (10^9) $ iterate spinCycle dish

  -- print 
  --   $ sum 
  --   $ map Set.size 
  --   $ take (10^1) 
  --   $ map getRoundRocks
  --   $ iterate spinCycle dish

  -- print $ score $ tilt north dish

  {-
  let dish = parse input
      (round_rocks, square_rocks) = dish
      bounds = (length $ head $ lines input, length $ lines input)
  let prefix = takeDistinct $ iterate (spinCycle bounds) dish
      dup = spinCycle bounds (last prefix)

      start = fromJust (elemIndex dup prefix)
      cycle_length = length prefix - start

      rest = (10^9 - start) `rem` cycle_length

  print $ score bounds $ (!! rest) $ iterate (spinCycle bounds) dup
  -}

  -- print $ score bounds $ (!! 100) $ iterate (spinCycle bounds) dish

  -- print $ score bounds $ spinCycle bounds dish 
  -- putStr "Part 1: "
  -- putStr "Part 2: "
