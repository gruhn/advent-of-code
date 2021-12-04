#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

main :: IO ()
main = do
    input <- readFile "02-input.txt"
    let parsed = parseInput input
    putStrLn "Part 1:"
    print $ fmap part1 parsed
    putStrLn "Part 2:"
    print $ fmap part2 parsed

parseInput :: String -> Maybe [Action]
parseInput = mapM (parseAction . words) . lines

data Action = Horizontal Int | Vertical Int 

parseAction :: [String] -> Maybe Action
parseAction action =
    case action of 
        ["forward", magnitude] -> Just (Horizontal $ read magnitude)
        ["down", magnitude]    -> Just (Vertical $ read magnitude)
        ["up", magnitude]      -> Just (Vertical $ -1 * read magnitude)
        _                      -> Nothing

part1 :: [Action] -> Int
part1 = uncurry (*) . foldl combine (0,0)
    where
        combine :: (Int, Int) -> Action -> (Int, Int)
        combine (x, y) (Horizontal a) = (x+a, y)
        combine (x, y) (Vertical a) = (x, y+a)

part2 :: [Action] -> Int
part2 = (\(x,y,aim) -> x*y) . foldl combine (0,0,0)
    where 
        combine :: (Int, Int, Int) -> Action -> (Int, Int, Int)
        combine (x, y, aim) (Horizontal x') = (x+x', y+aim*x', aim)
        combine (x, y, aim) (Vertical aim') = (x, y, aim+aim')