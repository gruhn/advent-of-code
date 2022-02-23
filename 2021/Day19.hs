module Day19 ( day19 ) where
import Text.Parsec.String (Parser, parseFromFile)
import Text.Parsec (option, digit, many1, sepBy, char, newline, count, endBy, sepBy1, sepEndBy)
import Text.Parsec.Char (string)
import Data.Matrix (Matrix, multStd, identity, transpose, mapRow)
import qualified Data.Matrix as Matrix
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (subtract)
import qualified Data.List as List
import Utils (integerP, converge, pairs)

beaconP :: Parser [Int]
beaconP = integerP `sepBy1` char ','

scannerP :: Parser (Set Point)
scannerP = do
    string "--- scanner "; integerP; string " ---"
    newline
    beacons <- map Beacon <$> beaconP `sepEndBy` newline
    return $ Set.fromList (Scanner [0,0,0] : beacons)

inputP :: Parser [Set Point]
inputP = scannerP `sepBy` newline
data Point = Scanner [Int] | Beacon [Int]
    deriving (Eq, Ord, Show)

modifyPoint :: ([Int] -> [Int]) -> Point -> Point
modifyPoint f (Scanner xs) = Scanner (f xs)
modifyPoint f (Beacon xs) = Beacon (f xs)

getCoords :: Point -> [Int]
getCoords (Scanner xs) = xs
getCoords (Beacon xs) = xs

data Axis = X | Y | Z

rotate90 :: Axis -> Matrix Int
rotate90 X = Matrix.fromLists
    [ [  1,  0,  0 ]
    , [  0,  0, -1 ]
    , [  0,  1,  0 ] ]
rotate90 Y = Matrix.fromLists
    [ [  0,  0, -1 ]
    , [  0,  1,  0 ]
    , [  1,  0,  0 ] ]
rotate90 Z = Matrix.fromLists
    [ [  0, -1,  0 ]
    , [  1,  0,  0 ]
    , [  0,  0,  1 ] ]

rotationActions :: [Matrix Int]
rotationActions = scanl (flip multStd) (identity 3)
    [ rotate90 X, rotate90 X, rotate90 X

    , rotate90 Y `multStd` rotate90 X
    , rotate90 X, rotate90 X, rotate90 X

    , rotate90 Y `multStd` rotate90 X
    , rotate90 X, rotate90 X, rotate90 X

    , rotate90 Y `multStd` rotate90 X
    , rotate90 X, rotate90 X, rotate90 X

    , rotate90 Z `multStd` rotate90 Y `multStd` rotate90 X
    , rotate90 X, rotate90 X, rotate90 X

    , rotate90 Z `multStd` rotate90 Z `multStd` rotate90 X
    , rotate90 X, rotate90 X, rotate90 X
    ]

applyAction :: Set Point -> Matrix Int -> Set Point
applyAction points matrix = 
    Set.map (modifyPoint (Matrix.toList . (matrix `multStd`) . Matrix.fromList 3 1)) points

orientations :: Set Point -> [Set Point]
orientations points = 
    map (applyAction points) rotationActions

translate :: [Int] -> Set Point -> Set Point
translate shift = Set.map $ modifyPoint (zipWith (+) shift)

isBeacon :: Point -> Bool
isBeacon (Beacon _) = True
isBeacon _ = False

sharedPoints :: Set Point -> Set Point -> Int
sharedPoints r1 r2 = Set.size (Set.intersection r1 r2)

subtract :: [Int] -> [Int] -> [Int]
subtract = zipWith (-)

align :: Set Point -> Set Point -> Maybe (Set Point)
align r1 r2 = 
    let beacons = map getCoords . Set.toList . Set.filter isBeacon
    in List.find ((12 <=) . sharedPoints r1)  $
        [ translate (vec1 `subtract` vec2) ori | 
            vec1 <- beacons r1,
            ori <- orientations r2, 
            vec2 <- beacons ori 
        ] 

alignAndMerge :: Set Point -> [Set Point] -> (Set Point, [Set Point])
alignAndMerge r1 [] = (r1, [])
alignAndMerge r1 (r2:rs) =
    case align r1 r2 of
        Just r2' -> alignAndMerge (Set.union r1 r2') rs
        Nothing  ->
            let (r1', rs') = alignAndMerge r1 rs
            in (r1', r2:rs')

overlap :: [Set Point] -> [Set Point]
overlap [] = []
overlap (r:rs) = 
    let (r', rs') = alignAndMerge r rs
    in r' : overlap rs'

manhattanDist :: [Int] -> [Int] -> Int
manhattanDist v w = 
    sum . map abs $ v `subtract` w

day19 :: IO ()
day19 = do
    input <- parseFromFile inputP "19-input.txt"

    case input of
        Left err -> print err
        Right regions -> do
            let result = Set.unions $ converge overlap regions
                (beacons, scanners) = Set.partition isBeacon result

            putStr "Part 1: "
            print $ Set.size beacons

            putStr "Part 2: "
            print 
                $ maximum 
                . map (uncurry manhattanDist) 
                . pairs . map getCoords 
                $ Set.toList scanners