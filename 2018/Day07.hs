module Main where

import ParseUtils (Parser, parseWith)
import Text.Megaparsec (sepEndBy)
import Text.Megaparsec.Char (newline, string, upperChar)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Data.Set (Set)

type Task = Char

parser :: Parser [(Task, Task)]
parser = 
  let
    line :: Parser (Task, Task)
    line = do
      string "Step "
      dependency <- upperChar
      string " must be finished before step "
      dependent <- upperChar
      string " can begin."
      return (dependency, dependent)
  in
    line `sepEndBy` newline

type DependencyMap = Map Task (Set Task)

toDependencyMap :: [(Task, Task)] -> DependencyMap
toDependencyMap pairs = 
  let
    tasks :: Set Task
    tasks = Set.fromList $ do
      (task1, task2) <- pairs
      [task1, task2]

    empty_deps :: DependencyMap
    empty_deps = Map.fromSet (const Set.empty) tasks 

    insert_dep :: (Task, Task) -> DependencyMap -> DependencyMap
    insert_dep (dependency, dependent) = Map.adjust (Set.insert dependency) dependent
  in 
    foldr insert_dep empty_deps pairs

-- | Remove dependency to the given `task` from all other tasks in the `DependencyMap`.
removeDependency :: Task -> DependencyMap -> DependencyMap
removeDependency task = Map.map (Set.delete task)

data State = State 
  { taskQueue :: Set Task
  , taskDeps  :: DependencyMap
  , busyTasks :: Set (Int, Task)
  , timeNow   :: Int
  } deriving Show

completionTimes :: DependencyMap -> (Task -> Int) -> Int -> [(Int, Task)]
completionTimes original_deps duration_of worker_count = 
  let
    update_queue :: State -> [(Int, Task)]
    update_queue state = 
      let
        (ready_tasks, rest_deps) = Map.partition Set.null state.taskDeps
        new_queue = state.taskQueue <> Map.keysSet ready_tasks
        new_state = state { taskQueue = new_queue, taskDeps = rest_deps }
      in
        start_tasks new_state

    start_tasks :: State -> [(Int, Task)]
    start_tasks state = 
      if Set.size state.busyTasks < worker_count then
        case Set.minView state.taskQueue of
          Nothing                 -> finish_task state
          Just (task, rest_queue) -> 
            let 
              completion_time = state.timeNow + duration_of task

              new_busy_tasks :: Set (Int, Task)
              new_busy_tasks = Set.insert (completion_time, task) state.busyTasks
            in
              start_tasks $ state { busyTasks = new_busy_tasks, taskQueue = rest_queue }
      else
        finish_task state

    finish_task :: State -> [(Int, Task)]
    finish_task state = 
      case Set.minView state.busyTasks of
        Nothing -> []
        Just ((time, task), rest_busy_tasks) -> 
          let
            new_state = state
              { busyTasks = rest_busy_tasks
              , taskDeps  = removeDependency task state.taskDeps
              , timeNow   = time
              }
          in
            (time, task) : update_queue new_state
  in
    update_queue (State Set.empty original_deps Set.empty 0)

main :: IO ()
main = do
  dependency_map <- toDependencyMap <$> parseWith parser "input/07.txt"

  putStr "Part 1: "
  print 
    $ map snd 
    $ completionTimes dependency_map (const 1) 1

  putStr "Part 2: "
  let task_durations = Map.fromList $ zip ['A' .. 'Z'] [61 ..]
  print 
    $ maximum
    $ map fst
    $ completionTimes dependency_map (task_durations Map.!) 5
