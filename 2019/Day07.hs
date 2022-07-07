module Main where

import Text.Megaparsec (parse, errorBundlePretty)
import Data.List (permutations)
import Data.Function (on)
import Data.Foldable (maximumBy, Foldable (foldl'))
import Data.Traversable (mapAccumL)

import IntcodeComputer

isInputDepleted :: State -> Bool
isInputDepleted state =
    case nextOp state of
        Inp _ -> null (input state)
        _     -> False

depletInput :: State -> State
depletInput s
    | isInputDepleted s = s
    | step s == s       = s
    | otherwise         = depletInput (step s)

appendInput :: [Int] -> State -> State
appendInput newInput s = 
    s { input = input s <> newInput }

accumOutput :: [Int] -> State -> ([Int], State)
accumOutput input state0 =
    let state1 = depletInput $ appendInput input state0
    in  (output state1, state1 { output = [] })

feedbackStep :: ([Int], [State]) -> ([Int], [State])
feedbackStep = uncurry (mapAccumL accumOutput)

initialState :: Program -> Int -> State
initialState program phase = State [phase] 0 program []

feedbackLoop :: Program -> [Int] -> Int
feedbackLoop program phaseSetting =
    let amplifiers = fmap (initialState program) phaseSetting
        finalOutput = head . input . head . snd
    in  finalOutput $ converge feedbackStep ([0], amplifiers)

main :: IO ()
main = do
    input <- parse parser "" <$> readFile "2019/input/07.txt"
    case input of
        Left error -> putStr (errorBundlePretty error)
        Right program -> do
            let phaseSettings1 = permutations [0..4]
                phaseSettings2 = permutations [5..9]

            putStr "Part 1: "
            print $ maximum $ fmap (feedbackLoop program) phaseSettings1

            putStr "Part 2: "
            print $ maximum $ fmap (feedbackLoop program) phaseSettings2