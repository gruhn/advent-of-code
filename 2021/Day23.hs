{-# LANGUAGE TupleSections #-}
module Day23 (day23) where
import Data.List (transpose, intersperse, partition)
import Data.Tree (Tree (Node), unfoldTree)
import Data.Maybe (mapMaybe)
import Control.Monad.State
import qualified Data.Set as Set

data Amphipod = A | B | C | D
    deriving Show

data Field = 
    Free | Occupied Amphipod | Room [Amphipod] 

type Board = [(Int, Field)]

instance Show Field where
    show Free = "."
    show (Occupied amph) = show amph
    show (Room amphs) = "."

showBoard :: Board -> String
showBoard board =
    let getRooms (Room xs) = xs
        getRooms _ = []

        rooms   = filter (not . null) . map (getRooms . snd) $ board
        
        line1 = "#############"
        line2 = ['#'] ++ concatMap (show . snd) board ++ ['#']
        line34 = transpose 
            $ ["# ", "# ", "##"] 
            ++ intersperse "##" (map (concatMap show) rooms) 
            ++ ["##", "# ", "# "]
        line5 = "  #########  "
    in unlines ([line1, line2] ++ line34 ++ [line5])


initialBoard :: Board
initialBoard = zip [0..]
    [ Free, Free 
    , Room [D,B]
    , Free
    , Room [C,A]
    , Free
    , Room [D,A]
    , Free
    , Room [B,C]
    , Free, Free
    ]

neighborhood :: Int -> Board -> Board
neighborhood index board =
    let (left', right') = splitAt index board
        left = reverse left'
        right = tail right'

        notOccupied (_, Occupied _) = False
        notOccupied _ = True

    in takeWhile notOccupied left 
    ++ takeWhile notOccupied right

reachable :: Int -> Int -> Board -> Bool
reachable from to board =
    elem to . map fst $ neighborhood from board

setAt :: Int -> a -> [(Int, a)] -> [(Int, a)]
setAt n x xs = 
    let front = take n xs
        rear  = drop (n+1) xs
    in front ++ [(n,x)] ++ rear

targetPos :: Amphipod -> Int
targetPos amph = case amph of 
    A -> 2; B -> 4; C -> 6; D -> 8

stepCost :: Amphipod -> Int
stepCost amph = case amph of 
    A -> 1; B -> 10; C -> 100; D -> 1000

buildSearchSpace :: Board -> (Board, [Board])
buildSearchSpace board =
    let pathCost :: Amphipod -> Int -> Int -> Int
        pathCost amp from to = abs (from - to) * stepCost amp
        
        finish :: (Int, Field) -> [Board]
        finish (index, Occupied amp) =
            let target = targetPos amp
                newBoard = setAt index Free board
            in [ newBoard | reachable index target board ]
        finish (index, Room (amp:amps)) =
            let target = targetPos amp
                newBoard = setAt index (Room amps) board
            in [ newBoard | reachable index target board ]
        finish _ = []

        spawn :: (Int, Field) -> [Board]
        spawn (index, Room (amp:amps)) =
            let isFree (_, Free) = True
                isFree _ = False

                occupy (target, _) = 
                    setAt target (Occupied amp) board
                
            in map occupy 
                $ filter isFree 
                $ neighborhood index board
        spawn _ = []
        
        next (_, Free) = []
        next (_, Room []) = []
        next pos = finish pos ++ spawn pos

    in (board, concatMap next board)

-- buildSearchSpace :: (Int, Board) -> (Int, [(Int, Board)])
-- buildSearchSpace (cost, board) =
--     let pathCost :: Amphipod -> Int -> Int -> Int
--         pathCost amp from to = abs (from - to) * stepCost amp
        
--         finish :: (Int, Field) -> [(Int, Board)]
--         finish (index, Occupied amp) =
--             let target = targetPos amp
--                 cost' = cost + pathCost amp index target
--                 newBoard = setAt index Free board
--             in [ (cost', newBoard) | reachable index target board ]
--         finish (index, Room (amp:amps)) =
--             let target = targetPos amp
--                 cost' = cost + pathCost amp index target
--                 newBoard = setAt index (Room amps) board
--             in [ (cost', newBoard) | reachable index target board ]
--         finish _ = []

--         spawn :: (Int, Field) -> [(Int, Board)]
--         spawn (index, Room (amp:amps)) =
--             let isFree (_, Free) = True
--                 isFree _ = False

--                 occupy (target, _) = 
--                     ( cost + pathCost amp index target
--                     , setAt target (Occupied amp) board )
                
--             in map occupy 
--                 $ filter isFree 
--                 $ neighborhood index board
--         spawn _ = []
        
--         next (_, Free) = []
--         next (_, Room []) = []
--         next pos = finish pos ++ spawn pos

--         allNext = concatMap next board 
--     in if null allNext then
--         (maxBound, [])
--     else 
--         (cost, allNext)

searchSpace :: Tree Board
searchSpace = unfoldTree buildSearchSpace initialBoard

isFinalState :: Board -> Bool
isFinalState board =
    let nonEmptyRoom (_, Room (_:_)) = True
        nonEmptyRoom _ = False
    in not (any nonEmptyRoom board)

-- bestPath :: Tree Int -> Int -> State Int [Board]
-- bestPath (Node cost []) bestCost = state ([],)
-- bestPath (Node cost bs) bestCost = state $ \bestCost -> 
--     if cost >= bestCost then
--         ([], bestCost)
--     else 
--         foldM bestPath bestCost bs

bestCost :: Tree Int -> Int -> Int
bestCost (Node cost []) best = min cost best
bestCost (Node cost bs) best
    | cost < best = foldr bestCost best bs
    | otherwise   = best

fixedCost :: Int
fixedCost =
    let unpackRoom (Room as) = Just as
        unpackRoom _ = Nothing

        rooms = mapMaybe (unpackRoom . snd) initialBoard

        startCost amps = sum 
            $ zipWith (*) [1..] 
            $ map stepCost amps

        startCosts = sum
            $ map startCost rooms

        closeCosts = sum (map stepCost [A,B,C,D]) * 3
    in startCosts + closeCosts

data Pos = Hallway Int | RoomFirst Int | RoomLast Int

type BoardState = Set.Set (Amphipod, Pos)

targetRoom :: Amphipod -> Int
targetRoom amph = case amph of 
    A -> 2; B -> 4; C -> 6; D -> 8

transitions :: BoardState -> [BoardState]
transitions state = 
    let isRoomFree :: Int -> Bool
        isRoomFree roomIndex =
            let matchRoom (_, RoomFirst idx) = idx == roomIndex
                matchRoom (_, RoomLast idx) = idx == roomIndex
                matchRoom _ = False
            in null $ Set.filter matchRoom state

        occupied :: [Int]
        occupied = 
            let hallwayPos (Hallway i) = [i]
                hallwayPos _ = []
            in concatMap hallwayPos $ Set.elems state

        reachableFrom :: Int -> Int -> [Int]
        reachableFrom from to =
            


        isOccupied :: Int -> Bool
        isOccupied hallwayIndex = 
            elem hallwayIndex occupied

        
        
        next :: (Amphipod, Pos) -> [BoardState]
        next s@(amph, Hallway index) =
            [ Set.delete s state | isRoomFree (targetRoom amph) state && reachable index (targetRoom amph) ]
    in concatMap next state 



day23 :: IO ()
day23 = 
    let unwrap (Node board _) = board
    in do
        putStr $ case searchSpace of 
            (Node board bs) -> unlines $ map (showBoard . unwrap) bs