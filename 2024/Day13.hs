module Main (main) where
import Utils (Parser, integer, parseFile, safeMinimum)
import Text.Megaparsec.Char (string, newline)
import Text.Megaparsec (sepBy, sepEndBy)
import Control.Monad (guard)
import Data.Foldable (traverse_)
import Debug.Trace (traceShowM)
import Data.Ratio (Ratio, (%))
import Data.Maybe (maybeToList, catMaybes)
import Theory.LinearArithmatic.BranchAndBound (branchAndBound)
import Theory.LinearArithmatic.Constraint (Constraint, Var, AffineExpr (AffineExpr), (.==), (.>=), Assignment, (.<=))
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet

data Vec = Vec 
  { x :: Rational
  , y :: Rational
  } deriving Show

type ClawMachineConfig = (Vec, Vec, Vec)

parser :: Parser [ClawMachineConfig]
parser = claw_machine_conifg `sepEndBy` newline
  where
    claw_machine_conifg :: Parser ClawMachineConfig
    claw_machine_conifg = do
      buttonA <- string "Button A: " *> button    <* newline
      buttonB <- string "Button B: " *> button    <* newline
      prize   <- string "Prize: "    *> prize_pos <* newline
      return (buttonA, buttonB, prize)

    button :: Parser Vec
    button = do 
      string "X+" 
      dx <- fromIntegral <$> integer 
      string ", Y+" 
      dy <- fromIntegral <$> integer
      return $ Vec dx dy

    prize_pos :: Parser Vec
    prize_pos = do 
      string "X=" 
      px <- fromIntegral <$> integer 
      string ", Y=" 
      py <- fromIntegral <$> integer
      return $ Vec px py

costA, costB :: Rational
costA = 3
costB = 1

varA, varB :: Int
varA = 1
varB = 2

eval :: Assignment -> Rational
eval assign = sum $ IntMap.unionWith (*) assign (IntMap.fromList [(varA, costA), (varB, costB)])

constant :: Rational -> AffineExpr
constant c = AffineExpr c IntMap.empty

toConstraints :: ClawMachineConfig -> [Constraint]
toConstraints (buttonA, buttonB, prize) = 
  let 
    factorA = AffineExpr 0 (IntMap.singleton 1 1)
    factorB = AffineExpr 0 (IntMap.singleton 2 1)
  in
    [ factorA * constant buttonA.x + factorB * constant buttonB.x .== constant prize.x
    , factorA * constant buttonA.y + factorB * constant buttonB.y .== constant prize.y
    , factorA .>= 0
    , factorB .>= 0
    ]   

solveOpt :: [Constraint] -> (Rational, Rational) -> Maybe Rational
solveOpt constraints (lo, hi) = 
  let
    factorA = AffineExpr 0 (IntMap.singleton varA 1)
    factorB = AffineExpr 0 (IntMap.singleton varB 1)

    lower_bound = constant lo .<= factorA
    upper_bound = factorA .<= constant hi
  in do
    sol <- branchAndBound (lower_bound : upper_bound : constraints) (IntSet.fromList [1,2])
    let value = eval sol
    if lo == hi then
      return value   
    else 
      return $ minimum $ (value:) $ catMaybes [ solveOpt constraints (lo, hi/2), solveOpt constraints (hi/2, lo) ]

main :: IO ()
main = do
  input <- parseFile parser "input/13.txt"

  putStr "Part 1: "
  print $ sum $ do
    config <- input
    maybeToList $ solveOpt (toConstraints config) (0, 100)

  putStr "Part 2: "
  print $ sum $ do
    (buttonA, buttonB, Vec prizeX prizeY) <- input
    let config' = (buttonA, buttonB, Vec (prizeX+10000000000000) (prizeY+10000000000000))
    maybeToList $ solveOpt (toConstraints config') (0, 10^20)
    
