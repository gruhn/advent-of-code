module Main where
import Text.Megaparsec (parse, errorBundlePretty)
import Data.List (permutations)
import Data.Function (on)
import Data.Foldable (maximumBy, Foldable (foldl'))
import Data.Traversable (mapAccumL)
import Lens.Micro (over, set)
import Lens.Micro.Extras (view)

import IntcodeComputer

accumOutput :: [Integer] -> State -> ([Integer], State)
accumOutput newInput state0 =
    let state1 = depletInput $ over input (<> newInput) state0
    in  (view output state1, set output [] state1)

feedbackStep :: ([Integer], [State]) -> ([Integer], [State])
feedbackStep = uncurry (mapAccumL accumOutput)

initialState' :: Program -> Integer -> State
initialState' program phase = initialState program [phase]

feedbackLoop :: Program -> [Integer] -> Integer
feedbackLoop program phaseSetting =
    let amplifiers = fmap (initialState' program) phaseSetting
        finalOutput = head . view input . head . snd
    in  finalOutput $ converge feedbackStep ([0], amplifiers)

main :: IO ()
main = do
    input <- parse parser "" <$> readFile "input/07.txt"
    case input of
        Left error -> putStr (errorBundlePretty error)
        Right program -> do
            let phaseSettings1 = permutations [0..4]
                phaseSettings2 = permutations [5..9]

            putStr "Part 1: "
            print $ maximum $ fmap (feedbackLoop program) phaseSettings1

            putStr "Part 2: "
            print $ maximum $ fmap (feedbackLoop program) phaseSettings2