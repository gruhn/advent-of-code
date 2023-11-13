module Main where
import Utils (Parser, integer, parseFile, safeMinimum, combinations, Vec3(Vec3))
import Text.Megaparsec (sepEndBy)
import Text.Megaparsec.Char (newline, string, char)
import Data.Foldable (minimumBy, Foldable (toList))
import Data.List (transpose)
import Control.Monad (guard)
import Data.Set (Set)
import Data.IntMap (IntMap)
import qualified Data.IntMap as Map
import qualified Data.Set as Set
import Data.Maybe (maybeToList)

data Particle = Particle
  { getPos :: Vec3 Int 
  , getVel :: Vec3 Int 
  , getAcc :: Vec3 Int
  , getIndex :: Int
  } deriving (Eq, Ord, Show)

setIndex :: Int -> Particle -> Particle
setIndex i p = p { getIndex = i }

setIndices :: [Particle] -> [Particle]
setIndices = zipWith setIndex [0..] 

parser :: Parser (Set Particle)
parser = Set.fromList . setIndices <$> particle `sepEndBy` newline
  where
    particle :: Parser Particle
    particle = do 
      pos <- string "p=" *> vec
      string ", "
      vel <- string "v=" *> vec
      string ", "
      acc <- string "a=" *> vec
      return $ Particle pos vel acc 0

    vec :: Parser (Vec3 Int)
    vec = Vec3
      <$  char '<'
      <*> integer <* char ','
      <*> integer <* char ','
      <*> integer <* char '>'

manhattanDist :: Vec3 Int -> Int
manhattanDist = sum . abs

-- | In the limit accerleration dominates and initial velociy/position don't matter.
--   For particles with the same acceleration, velocity dominates. Only if velocity
--   is also the same, we have to compare positions. We get such an ordering by 
--   comparig lists of magnitudes, where dominating values come first, because lists 
--   are ordered lexicographically.
limitDist :: Particle -> Particle -> Ordering
limitDist (Particle posA velA accA _) (Particle posB velB accB _) = 
  compare (map manhattanDist [accA, velA, posA]) 
          (map manhattanDist [accB, velB, posB])

-- | Compute solutions of: ax^2 + bx + c = 0
--   Annoying special case that is not handled here:
--   if all coefficients are zero, any `Double` is a solution.
solveQuadratic :: Double -> Double -> Double -> [Double]
solveQuadratic 0 0 _ = []
solveQuadratic 0 b c = [-c/b]
solveQuadratic a b c = do
  sign <- [1, -1]
  let x = ((-b) + sign * sqrt (b^2 - 4*a*c)) / (2*a)
  -- reject complex solutions
  guard $ not $ isNaN x
  return x

-- | A particles trajactory is described by 
--
--      pos + (vel + acc/2)*time + (acc/2) * time^2
--   
--   Collisions are given by equating two particle trajactories and solving for time.
collisionTime :: Particle -> Particle -> Maybe Int
collisionTime (Particle posA velA accA _) (Particle posB velB accB _) = do
  let collision_times_per_component :: [[Int]]
      collision_times_per_component = do
        -- For each component x,y,z we compute the coefficients a,b,c of a 
        -- quadratic equation that gives us the time points where the two 
        -- particles meet:
        [acc_diff, vel_diff, pos_diff] <- transpose
          [ toList $ accA - accB
          , toList $ velA - velB
          , toList $ posA - posB ]

        let a = fromIntegral acc_diff / 2
            b = fromIntegral acc_diff / 2 + fromIntegral vel_diff
            c = fromIntegral pos_diff
        
        -- If all coefficents are zero, any time value is a solution, 
        -- so we must ignore that component.
        guard $ any (/= 0) [a,b,c]

        return $ do
          time <- solveQuadratic a b c
          -- reject negative solutions ...
          guard $ time >= 0
          -- ... and non-integer solutions since they don't make sense in this context.
          let int_time = round time :: Int
          guard $ time == fromIntegral int_time
          return int_time

  case collision_times_per_component of
    -- If there are no concrete solutions for any component, then all components got
    -- filtered out because the computed coefficients were all zero. That can only 
    -- happen if both particles have no acceleration, no velosity, and are positioned 
    -- at the origin. Thus, they are perpetually colliding at every time step. 
    -- Since we are interested in the earliest collisions, we just return time step zero:
    [] -> Just 0

    -- If there are concrete solutions for at least one component, we need to identify 
    -- solutions that are the same across all components. Otherwise, the represent points
    -- in time where the particles meet in say two dimensions but not the third.
    (first_component_times : other_component_times) -> safeMinimum $ do
      time <- first_component_times

      -- and make sure that `time` is a solution in all dimensions.
      guard $ all (time `elem`) other_component_times

      return time

-- | Keep all particles that are eliminated by collisions at some time step.
filterColliding :: Set Particle -> Set Particle
filterColliding particles = distinct_collisions collision_schedule
  where
    -- Group particles by time point where they are involved 
    -- in a potential collision. This may still include particles 
    -- that won't actually collide because the collision partner 
    -- was already eliminated in a prior time step.
    collision_schedule :: IntMap (Set Particle)
    collision_schedule = Map.fromListWith (<>) $ do
      (p1, p2) <- combinations $ Set.toList particles
      collision_time <- maybeToList $ collisionTime p1 p2
      return (collision_time, Set.fromList [p1,p2])

    reject_prior_collisions :: Set Particle -> IntMap (Set Particle) -> IntMap (Set Particle)
    reject_prior_collisions prior_collisions = Map.map (Set.\\ prior_collisions)

    distinct_collisions :: IntMap (Set Particle) -> Set Particle
    distinct_collisions schedule = 
      -- Find the earliest time step in the schedule, because all collisions in this time 
      -- step can't be prevented by earlier collisions:
      case Map.minView schedule of
        -- If there is no earliest time step, the schedule is empty and there are no more collisions.
        Nothing -> Set.empty
        -- Otherwise extract the particles that collide in this time step and remove them from any
        -- future time step in the schedule, since they can't be involved in future collisions anymore.
        -- Then iterate until no more time steps are left.
        Just (colliding_particles, rest_schedule) -> 
          colliding_particles <> distinct_collisions 
            (reject_prior_collisions colliding_particles rest_schedule)


------------ FOR TESTING -----------------

-- posAt :: Int -> Particle -> Vec3 Int
-- posAt time (Particle pos vel acc _) = 
--   let time_vec = Vec3 time time time in
--   pos + (time_vec * vel) + ((`div` 2) <$> (time_vec+1) * time_vec) * acc

-- trajectory :: Particle -> [Vec3 Int]
-- trajectory  (Particle pos vel acc index) = 
--   pos : trajectory  (Particle (pos+vel+acc) (vel+acc) acc index)

-----------------------------------------

main :: IO ()
main = do 
  particles <- parseFile parser "input/20.txt"

  -- putStr "Test: "
  -- print $ and $ do 
  --   p <- toList particles
  --   (t, pos) <- zip [0..10] (trajectory p)
  --   return $ pos == posAt t p
    
  putStr "Part 1: "
  print $ getIndex $ minimumBy limitDist particles

  putStr "Part 2: "
  print $ Set.size $ particles Set.\\ filterColliding particles
