module Main (main) where
import Utils (Parser, integer, parseFile)
import Text.Megaparsec.Char (string, newline)
import Text.Megaparsec (sepEndBy)
import Data.Maybe (maybeToList)
import Control.Monad (guard)

data Vec = Vec { x :: Rational, y :: Rational } 
  deriving Show

type ClawMachineConfig = (Vec, Vec, Vec)

parser :: Parser [ClawMachineConfig]
parser = claw_machine_config `sepEndBy` newline
  where
    claw_machine_config :: Parser ClawMachineConfig
    claw_machine_config = (,,)
      <$ string "Button A: " <*> button <* newline
      <* string "Button B: " <*> button <* newline
      <* string "Prize: "    <*> prize  <* newline

    rational :: Parser Rational
    rational = fromIntegral <$> integer

    button :: Parser Vec
    button = Vec 
      <$ string "X+" <*> rational <* string ", "
      <* string "Y+" <*> rational

    prize :: Parser Vec
    prize = Vec 
      <$ string "X=" <*> rational <* string ", "
      <* string "Y=" <*> rational

isIntegral :: Rational -> Bool
isIntegral num = fromIntegral (floor num) == num

solve :: ClawMachineConfig -> Maybe (Integer, Integer)
solve (buttonA, buttonB, prize) = do
  -- Gauss elimination:
  let factorA_denom = buttonB.y * buttonA.x - buttonB.x * buttonA.y
  guard $ factorA_denom /= 0
  let factorA = (prize.x * buttonB.y - prize.y * buttonB.x) / factorA_denom
  guard $ buttonB.x /= 0 
  let factorB = (prize.x - buttonA.x * factorA) / buttonB.x
  -- Check additional constraints:
  guard $ factorA >= 0 && isIntegral factorA
  guard $ factorB >= 0 && isIntegral factorB
  return (floor factorA, floor factorB)

adjustPrize :: (Rational -> Rational) -> ClawMachineConfig -> ClawMachineConfig
adjustPrize adjust (buttonA, buttonB, prize) = 
  (buttonA, buttonB, Vec (adjust prize.x) (adjust prize.y))

main :: IO ()
main = do
  input <- parseFile parser "input/13.txt"

  let cost :: Integer -> Integer -> Integer
      cost factorA factorB = 3 * factorA + factorB

  putStr "Part 1: "
  print $ sum $ do
    config <- input
    (factorA, factorB) <- maybeToList $ solve config
    guard $ factorA <= 100
    guard $ factorB <= 100
    return $ cost factorA factorB

  putStr "Part 2: "
  print $ sum $ do
    config <- adjustPrize (+10000000000000) <$> input
    (factorA, factorB) <- maybeToList $ solve config
    return $ cost factorA factorB
