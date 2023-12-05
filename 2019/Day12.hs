module Main where
import Utils (Parser, Vec3(..), integer, parseFile, takeDistinct)
import Text.Megaparsec (sepEndBy)
import Text.Megaparsec.Char (newline, string, char)
import Data.Foldable (Foldable (toList), traverse_)

type Moon = (Vec3 Int, Vec3 Int)

parser :: Parser [Vec3 Int]
parser = vec `sepEndBy` newline
  where
    vec :: Parser (Vec3 Int)
    vec = do
      char '<'
      x <- string "x=" *> integer <* string ", "
      y <- string "y=" *> integer <* string ", "
      z <- string "z=" *> integer
      char '>'
      return $ Vec3 x y z

applyGravity :: [Moon] -> [Moon]
applyGravity moons = do
  (moon_pos, moon_vel) <- moons

  let delta :: Vec3 Int
      delta = sum $ do
        (other_moon_pos, _) <- moons
        return $ signum $ other_moon_pos - moon_pos

  return (moon_pos, moon_vel + delta)
  
applyVelocity :: [Moon] -> [Moon]
applyVelocity moons = do
  (moon_pos, moon_vel) <- moons
  return (moon_pos + moon_vel, moon_vel)

step :: [Moon] -> [Moon]
step = applyVelocity . applyGravity

energy :: Moon -> Int
energy (moon_pos, moon_vel) = potential * kinetic
  where
    potential = sum $ toList $ abs moon_pos
    kinetic   = sum $ toList $ abs moon_vel

main :: IO ()
main = do
  positions <- parseFile parser "input/12.txt"

  let start_velocity :: Vec3 Int
      start_velocity = Vec3 0 0 0

      moons :: [Moon]
      moons = map (,start_velocity) positions

  putStr "Part 1: "
  print $ sum $ map energy $ iterate step moons !! 100

  putStrLn "Part 2: "
  print $ length $ takeDistinct $ iterate step moons

  -- print $ length $ takeDistinct $ map (snd . head) $ iterate step moons
  -- traverse_ print $ iterate step moons !! 2772

