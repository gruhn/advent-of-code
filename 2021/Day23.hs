module Main where
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.List as List
import Algorithm.Search (aStarAssoc, dijkstraAssoc)
import Data.Foldable
import Data.Maybe (fromJust, mapMaybe)

data Amphipod = A | B | C | D
    deriving (Show, Eq, Ord)

type Pos = (Int, Int)

type Board = Map Pos Amphipod

reachable :: Set Pos -> Board -> Pos -> Set Pos
reachable area board start = Set.filter canHalt $ go start Set.empty where
    notRoomEntry (x,0) = x `notElem` [2,4,6,8]
    notRoomEntry _ = True

    canHalt pos = pos /= start && notRoomEntry pos

    go :: Pos -> Set Pos -> Set Pos
    go pos@(x,y) seenPos =
        let isOutside = not $ pos `Set.member` area
            alreadySeen = pos `Set.member` seenPos
            isOccupied  = pos `Map.member` board && pos /= start

            neighbors = Set.fromList
                [ (x-1,y), (x+1,y), (x,y-1), (x,y+1) ]
        in if isOutside || alreadySeen || isOccupied then
            seenPos
        else 
            foldr go (Set.insert pos seenPos) neighbors

targetRoomIndex :: Amphipod -> Int
targetRoomIndex amph = case amph of
    A -> 2; B -> 4; C -> 6; D -> 8

targetPos :: Amphipod -> Set Pos
targetPos amph = Set.fromList
    [ (targetRoomIndex amph, y) | y <- [-4 .. -1] ]

hallway :: Set Pos
hallway = Set.fromList
    [ (x,0) | x <- [0..10] ] 

rooms :: Set Pos
rooms = Set.fromList
    [ (x,y) | y <- [-4 .. -1], x <- [2,4,6,8] ]

boardArea :: Set Pos
boardArea = Set.union hallway rooms

distance :: Pos -> Pos -> Int
distance (x1, y1) (x2, y2) =
    if x1 == x2 then
        abs (y1 - y2)
    else
        abs (x1 - x2) + abs y1 + abs y2

stepCost :: Amphipod -> Int
stepCost amph = case amph of 
    A -> 1; B -> 10; C -> 100; D -> 1000

costLowerBound :: Board -> Int
costLowerBound board = 
    let pathCost :: Pos -> Amphipod -> Int
        pathCost (x,y) amph
            -- already in target room => count as negative cost
            | x == targetRoomIndex amph = y * stepCost amph
            | otherwise = distance (x,y) (targetRoomIndex amph, 0) * stepCost amph

        moveToRoomCost = sum . Map.mapWithKey pathCost $ board 
        enterRoomCost = sum . fmap (((4+3+2+1) *) . stepCost) $ [A,B,C,D]

    in moveToRoomCost + enterRoomCost

move :: Board -> Pos -> Pos -> Board
move board from to = 
    let fromVal = Map.lookup from board 
        toVal = Map.lookup to board 
    in case (fromVal, toVal) of
        (Nothing, _) -> error "trying to move empty position"
        (_, Just _) -> error "trying overwrite non-empty position"
        (Just amph, Nothing) -> 
            Map.delete from . Map.insert to amph $ board

nextStates :: Board -> [(Board, Int)]
nextStates board =
    let isHallway (x,0) = True
        isHallway _ = False
        
        next :: (Pos, Amphipod) -> [(Board, Int)]
        next (pos@(x,y), amph) =  
            let otherRooms = rooms Set.\\ 
                    if all (== amph) $ mapMaybe (`Map.lookup` board) $ Set.toList $ targetPos amph then
                        targetPos amph
                    else 
                        Set.empty
                span = reachable boardArea board pos Set.\\ otherRooms
            in map (stateCost (pos, amph)) . Set.toList $ 
                if isHallway pos then
                    Set.intersection (targetPos amph) span
                else
                    span

        stateCost :: (Pos, Amphipod) -> Pos -> (Board, Int)
        stateCost (from, amph) to =
            let newBoard = move board from to
                cost = distance from to * stepCost amph
            in (newBoard, cost)

    in concatMap next $ Map.toAscList board

initialState :: Board 
initialState = Map.fromList 
    [ ((2,-1),D), ((2,-2),D), ((2,-3),D), ((2,-4),B)
    , ((4,-1),C), ((4,-2),C), ((4,-3),B), ((4,-4),A)
    , ((6,-1),D), ((6,-2),B), ((6,-3),A), ((6,-4),A)
    , ((8,-1),B), ((8,-2),A), ((8,-3),C), ((8,-4),C)
    ]

finalState :: Board 
finalState = Map.fromList 
    [ ((2,-1),A), ((2,-2),A), ((2,-3),A), ((2,-4),A)
    , ((4,-1),B), ((4,-2),B), ((4,-3),B), ((4,-4),B)
    , ((6,-1),C), ((6,-2),C), ((6,-3),C), ((6,-4),C)
    , ((8,-1),D), ((8,-2),D), ((8,-3),D), ((8,-4),D)
    ]

main :: IO ()
main = do
    let (cost, path) = fromJust $ aStarAssoc nextStates costLowerBound (== finalState) initialState
    putStr $ unlines . map showBoard $ path
    print cost

showBoard :: Board -> String
showBoard board =
    let board' = Map.map show board
        showPos pos = Map.findWithDefault "." pos (Map.map show board)

        go (11,-4) = ""
        go (11,y) = '\n' : go (0,y-1)
        go (x,y)  = showPos (x,y) ++ go (x+1,y)
    in '\n' : go (0,0)