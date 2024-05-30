module Main where
import ParseUtils (parseWith, Parser, symbol)
import Text.Megaparsec (sepEndBy, between, choice)
import Text.Megaparsec.Char (newline, char, string)
import Text.Megaparsec.Char.Lexer (decimal)
import qualified Data.List as List
import qualified Data.Time as Time
import Data.Time (UTCTime (UTCTime), TimeOfDay (TimeOfDay))
import Data.Ord (comparing)
import Data.Maybe (mapMaybe)
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

data GuardState = Asleep | Awake

type GuardID = Int

data LogEntry = GuardBeginsShift GuardID | GuardStateChange GuardState

parser :: Parser [(UTCTime, LogEntry)]
parser = 
  let
    falls_asleep :: Parser LogEntry
    falls_asleep = GuardStateChange Asleep <$ string "falls asleep"

    wakes_up :: Parser LogEntry
    wakes_up = GuardStateChange Awake <$ string "wakes up"

    shift_begins :: Parser LogEntry
    shift_begins = do
      string "Guard #" 
      guard_id <- decimal 
      string " begins shift"
      return (GuardBeginsShift guard_id)

    log_time :: Parser UTCTime
    log_time = between (symbol "[") (symbol "]") $ do
      year   <- decimal <* char '-'
      month  <- decimal <* char '-'
      day    <- decimal <* char ' '
      let date = Time.fromGregorian year month day
      hour   <- decimal <* char ':'
      minute <- decimal
      let time_of_day = Time.timeOfDayToTime $ TimeOfDay hour minute 0
      return $ UTCTime date time_of_day

    line :: Parser (UTCTime, LogEntry)
    line = do 
      time  <- log_time
      entry <- choice [falls_asleep, wakes_up, shift_begins]
      return (time, entry)
  in
    line `sepEndBy` newline

type Duration = (UTCTime, UTCTime)

sleepDurationsPerGuard :: [(UTCTime, LogEntry)] -> IntMap [Duration]
sleepDurationsPerGuard log_orignial = 
  let
    log_chronological :: [(UTCTime, LogEntry)]
    log_chronological = List.sortBy (comparing fst) log_orignial

    log_with_end_times :: [(UTCTime, LogEntry, UTCTime)]
    log_with_end_times = zipWith go log_chronological (tail log_chronological)
      where
        go (start_time, entry) (end_time, _) = (start_time, entry, end_time)

    log_grouped_by_shifts :: [[(UTCTime, LogEntry, UTCTime)]]
    log_grouped_by_shifts = List.groupBy same_shift log_with_end_times
      where
        same_shift _ (_, GuardBeginsShift _, _) = False
        same_shift _ _                          = True

    sleep_duration_from :: (UTCTime, LogEntry, UTCTime) -> Maybe Duration
    sleep_duration_from (start_time, guard_state, end_time) = 
      case guard_state of 
        GuardBeginsShift _      -> Nothing -- guard is assumed to be awake when beginning the shift
        GuardStateChange Awake  -> Nothing
        GuardStateChange Asleep -> Just (start_time, end_time)

    sleep_durations_in_shift :: [(UTCTime, LogEntry, UTCTime)] -> Maybe (GuardID, [Duration])
    sleep_durations_in_shift =
      \case
        [] -> error "got a shift without entires ==> shouldn't happen"
        ((_, GuardBeginsShift guard_id, _) : rest) -> Just (guard_id, mapMaybe sleep_duration_from rest)
        -- If first entry is not a `GuardBeginsShift` then the `Asleep` entries can't be 
        -- attributed to any guard so we just ignore them. This can happen for the first shift
        -- in the log, because the very first entry may not be a `GuardBeginsShift` entry:
        _ -> Nothing 
  in
    IntMap.fromListWith (++) $ mapMaybe sleep_durations_in_shift log_grouped_by_shifts

toMinutes :: Duration -> Int
toMinutes (startTime, endTime) = 
  round $ Time.nominalDiffTimeToSeconds $ Time.diffUTCTime endTime startTime / 60

maxSleepGuard :: IntMap [Duration] -> GuardID
maxSleepGuard naps_per_guard =
  fst $ List.maximumBy (comparing snd) $ IntMap.toList $ IntMap.map (sum . map toMinutes) naps_per_guard

mostFrequentSleepMinute :: [Duration] -> Int
mostFrequentSleepMinute durs = head $ List.maximumBy (comparing length) $ List.group $ List.sort $ do
  (startTime, endTime) <- durs
  let start_min = (Time.timeToTimeOfDay startTime.utctDayTime).todMin
      end_min   = (Time.timeToTimeOfDay endTime.utctDayTime).todMin -1
  [ start_min .. end_min  ]

mostFrequentSleepMinute' :: IntMap [Duration] -> (GuardID, Int)
mostFrequentSleepMinute' naps =
  fst $ List.maximumBy (comparing snd) $ Map.toList $ Map.fromListWith (+) $ do
    (guard_id, durs) <- IntMap.toList naps
    (startTime, endTime) <- durs
    let start_min = (Time.timeToTimeOfDay startTime.utctDayTime).todMin
        end_min   = (Time.timeToTimeOfDay endTime.utctDayTime).todMin -1
    minute <- [ start_min .. end_min  ]
    return ((guard_id, minute), 1)

  -- List.maximumBy (comparing snd) $ IntMap.toList $ IntMap.fromListWith (+) $ do
  --   (startTime, endTime) <- durs
  --   let start_min = (Time.timeToTimeOfDay startTime.utctDayTime).todMin
  --       end_min   = (Time.timeToTimeOfDay endTime.utctDayTime).todMin -1
  --   minute <- [ start_min .. end_min  ]
  --   return (minute, 1)

-- 150113 too high

main :: IO ()
main = do
  log_entries <- parseWith parser "input/04.txt"
  let naps = sleepDurationsPerGuard log_entries

  putStr "Part 1: "
  let guard_id = maxSleepGuard naps
      minute = mostFrequentSleepMinute $ naps IntMap.! guard_id
  print $ guard_id * minute
  print $ (guard_id, minute)

  putStr "Part 2: "
  print $ uncurry (*) $ mostFrequentSleepMinute' $ naps

