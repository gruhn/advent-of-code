module Main where
import qualified Data.List as L
import Control.Monad (guard)
import Algorithm.Search (dijkstra)

data ObjectPair = OPair
  { generatorFloor :: Int
  , microchipFloor :: Int
  } deriving (Eq, Ord, Show)

data Object = Obj 
  { pairID :: Int 
  , currentFloor :: Int
  , isGenerator :: Bool
  } deriving (Eq, Show)

unpair :: [ObjectPair] -> [Object] 
unpair = concat . L.zipWith go [0..]
  where
    go :: Int -> ObjectPair -> [Object]
    go pair_id (OPair gen_floor chip_floor) = 
      [Obj pair_id gen_floor True, Obj pair_id chip_floor False]

repair :: [Object] -> [ObjectPair]
repair objects = L.sort $ L.zipWith go generators' microchips'
  where
    (generators, microships) = L.partition isGenerator objects

    generators' = L.sortOn pairID generators
    microchips' = L.sortOn pairID microships

    go gen chip
      | pairID gen /= pairID chip = error "pair IDs don't match"
      | otherwise = OPair (currentFloor gen) (currentFloor chip)

data State = State
  { elevatorFloor :: Int
  , objectPairs :: [ObjectPair]
  } deriving (Eq, Ord, Show)

allGeneratorFloors :: [ObjectPair] -> [Int]
allGeneratorFloors = fmap generatorFloor

allMicrochipFloors :: [ObjectPair] -> [Int]
allMicrochipFloors = fmap microchipFloor

setFloor :: Int -> Object -> Object
setFloor floor obj = obj { currentFloor = floor }

hasConflict :: State -> Bool
hasConflict (State _ object_pairs) = any chip_in_conflict object_pairs
  where
    chip_in_conflict :: ObjectPair -> Bool
    chip_in_conflict (OPair gen_floor chip_floor) =
      gen_floor /= chip_floor && chip_floor `L.elem` allGeneratorFloors object_pairs

nextStates :: State -> [State]
nextStates (State elevator_floor object_pairs) = do
  next_floor <- [elevator_floor+1, elevator_floor-1]
  guard (1 <= next_floor && next_floor <= 4)

  let objects = unpair object_pairs
      objects_on_current_floor = filter ((elevator_floor ==) . currentFloor) objects

  picked_up_objects <- L.subsequences objects_on_current_floor
  guard (length picked_up_objects `L.elem` [1,2])

  let picked_up_objects_updated = setFloor next_floor <$> picked_up_objects

      objects_updated = (objects L.\\ picked_up_objects) <> picked_up_objects_updated
      object_pairs_updated = repair objects_updated

      new_game_state = State next_floor object_pairs_updated

  guard (not $ hasConflict new_game_state)

  return new_game_state

isFinalState :: State -> Bool
isFinalState (State _ object_pairs) =
  all (== 4) (currentFloor <$> unpair object_pairs)

stateTransitionCost :: State -> State -> Int
stateTransitionCost _ _ = 1

search :: [ObjectPair] -> Maybe (Int, [State])
search objects = dijkstra nextStates stateTransitionCost isFinalState
  $ State { elevatorFloor = 1, objectPairs = L.sort objects }

main :: IO ()
main = do
  let input_object_pairs = 
        [ OPair 1 1 -- promethium
        , OPair 2 3 -- cobalt 
        , OPair 2 3 -- curium 
        , OPair 2 3 -- ruthenium 
        , OPair 2 3 -- plutonium 
        ]

  putStr "Part 1: "
  print $ fst <$> search input_object_pairs

  putStr "Part 2: "
  let added_object_pairs = [ OPair 1 1, OPair 1 1 ]
  print $ fst <$> search (input_object_pairs <> added_object_pairs)