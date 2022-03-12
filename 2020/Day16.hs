module Day16 where

import Text.Parsec
    ( char, digit, newline, noneOf, string, many1, sepBy, many )
import Text.Parsec.String (parseFromFile, Parser)
import Data.Either (fromRight)
import Data.List (transpose, sortBy, (\\), isPrefixOf)
import Data.Function (on)
import Data.Bifunctor (bimap, Bifunctor (first))

type Range = (Int, Int)
type Domain = [Range]
type Rule = (String, Domain)
type Ticket = [Int]

parser :: Parser ([Rule], Ticket, [Ticket])
parser =
    let nat :: Parser Int
        nat = read <$> many1 digit

        range :: Parser Range
        range = do
            a <- nat
            char '-'
            b <- nat
            return (a,b)

        rule :: Parser Rule
        rule = do
            field <- many1 (noneOf ":\n")
            string ": "
            range1 <- range
            string " or "
            range2 <- range
            newline
            return (field, [range1, range2])

        rules :: Parser [Rule]
        rules = many rule

        ticket :: Parser Ticket
        ticket = nat `sepBy` char ','

        tickets :: Parser [Ticket]
        tickets = ticket `sepBy` newline
    in do
        rs <- rules
        newline
        string "your ticket:"
        newline
        yourTicket <- ticket
        newline
        newline
        string "nearby tickets:"
        newline
        nearbyTickets <- tickets

        return (rs, yourTicket, nearbyTickets)

inRange :: Int -> Range -> Bool
inRange value (a,b) =
    a <= value && value <= b

inDomain :: Int -> Domain -> Bool
inDomain value = any (inRange value)

isValidValue :: [Rule] -> Int -> Bool
isValidValue rules value =
    value `inDomain` concatMap snd rules

matchFieldColumn :: [Rule] -> [Ticket] -> [[(String, Int)]]
matchFieldColumn rules tickets =
    let columns = transpose tickets
        fieldMatch (field, domain) =
            (field, [ i | (i, col) <- zip [0..] columns, all (`inDomain` domain) col ])

        search :: [(String, [Int])] -> [(String, Int)] -> [[(String, Int)]]
        search [] res = [res]
        search ((field, is) : rest) accum =
            let is' = is \\ map snd accum
            in filter (not . null) $ concatMap (\i -> search rest ((field, i) : accum)) is'
    in flip search []
        $ sortBy (compare `on` length . snd)
        $ map fieldMatch rules


solve :: ([Rule], Ticket, [Ticket]) -> IO ()
solve (rules, yourTicket, tickets) = do
    putStr "Part 1: "
    let invalidValues = filter (not . isValidValue rules)
    print
        $ sum
        . concatMap invalidValues
        $ tickets

    putStr "Part 2: "
    let validTickets = filter (all (isValidValue rules)) tickets
    print
        $ product
        . map fst
        . filter (\(_, field) -> "departure" `isPrefixOf` field)
        . zip yourTicket
        . map fst
        . sortBy (compare `on` snd)
        . head
        $ matchFieldColumn rules (yourTicket : validTickets)

fromEither :: Either a a -> a
fromEither (Left a) = a
fromEither (Right a) = a

day16 :: IO ()
day16 = parseFromFile parser "input/16.txt" 
    >>= fromEither . bimap print solve