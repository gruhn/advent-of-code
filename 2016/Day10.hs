module Main where

import Utils (Parser, parseHardError, converge)
import Text.Megaparsec.Char (newline, string)
import Text.Megaparsec (sepBy, (<|>), sepBy1)
import Text.Megaparsec.Char.Lexer (decimal)
import qualified Data.Map as M
import qualified Data.List as L
import Data.Function ((&))
import Data.Maybe (isNothing, isJust)

data Target = Bot Int | Bin Int
  deriving (Eq, Ord, Show)

data Instr = Inp Int Target | Trans Target Target Target
  deriving (Eq, Show)

parser :: Parser [Instr]
parser = instr `sepBy1` newline
  where
    instr = input <|> transition

    bot = Bot <$ string "bot "    <*> decimal
    bin = Bin <$ string "output " <*> decimal

    target = bot <|> bin

    -- value 23 goes to bot 8
    input = Inp
      <$ string "value "    <*> decimal
      <* string " goes to " <*> target

    -- bot 128 gives low to bot 56 and high to bot 91
    transition = Trans
      <$> target <* string " gives low to "
      <*> target <* string " and high to "
      <*> target

type OwnerMap = M.Map Target [Int]

apply :: (OwnerMap, [Instr]) -> (OwnerMap, [Instr])
apply (initial_owner_map, instrs) = foldr go (initial_owner_map, []) instrs
  where
    go :: Instr -> (OwnerMap, [Instr]) -> (OwnerMap, [Instr])
    go (Inp num target) (owner_map, rest_instrs) =
      (M.insertWith (++) target [num] owner_map, rest_instrs)
    go instr@(Trans from low_to high_to) (owner_map, rest_instrs) =
      case M.lookup from owner_map of
        Just [num1,num2] -> (new_owner_map, rest_instrs)
          where
            low  = min num1 num2
            high = max num1 num2

            new_owners = M.fromList [ (low_to, [low]), (high_to, [high]) ]
            new_owner_map = M.unionWith (++) owner_map new_owners

        _ -> (owner_map, instr : rest_instrs)

findTargetOf :: OwnerMap -> [Int] -> Maybe Target
findTargetOf owner_map nums = fst <$> L.find go (M.toList owner_map)
  where
    go (_, nums') = L.sort nums' == L.sort nums

getOutput :: OwnerMap -> Int -> Maybe [Int]
getOutput owner_map bin = M.lookup (Bin bin) owner_map

main :: IO ()
main = do
  instructions <- parseHardError parser <$> readFile "input/10.txt"

  let (owner_map, _) = converge apply (mempty, instructions)

  putStr "Part 1: "
  print $ findTargetOf owner_map [17,61]

  putStr "Part 2: "
  print $ product . concat <$> traverse (getOutput owner_map) [0,1,2]