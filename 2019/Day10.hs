module Main where
import Text.Megaparsec (Parsec, (<|>), many, sepBy, errorBundlePretty, parse)
import Data.Void (Void)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Text.Megaparsec.Char (char, newline)
import Data.Foldable (maximum, maximumBy)
import Data.Function (on)
import Data.List (sortBy, transpose, delete)

type Point = (Integer, Integer)

grid :: [[Point]]
grid = (<$> [0..]) . flip (,) <$> [0..]

zip2D :: [[a]] -> [[b]] -> [[(a,b)]]
zip2D = zipWith zip

filter2D :: (a -> Bool) -> [[a]] -> [[a]]
filter2D p = fmap (filter p)

noDuplicates :: Ord a => [a] -> [a]
noDuplicates = Set.toList . Set.fromList

partitionBy :: Ord b => (a -> b) -> [a] -> [[a]]
partitionBy classOf =
    let insertByClass a = Map.insertWith (<>) (classOf a) [a]
    in  fmap snd . Map.toList . foldr insertByClass Map.empty

parser :: Parsec Void String [Point]
parser =
    let cell = (True <$ char '#') <|> (False <$ char '.')
        row = many cell

        points :: [[Bool]] -> [Point]
        points = fmap fst . concat . filter2D snd . zip2D grid

    in  points <$> row `sepBy` newline

-- >>> angle (0,1)
-- >>> pi/2
-- >>> angle (1,0)
-- 1.5707964
-- 1.5707963267948966
-- 0.0

angle :: Point -> Float
angle (x0,y0)
    | x > 0           = atan (y/x)
    | x < 0 && y >= 0 = atan (y/x) + pi
    | x < 0 && y < 0  = atan (y/x) - pi
    | x == 0 && y > 0 = pi / 2
    | x == 0 && y < 0 = - pi / 2
    | otherwise       = undefined 
    where 
        x = fromInteger x0
        y = fromInteger y0

translate :: Point -> [Point] -> [Point]
translate (x0,y0) = fmap (\(x,y) -> (x-x0, y-y0))

visibleFrom :: [Point] -> Point -> Int
visibleFrom points origin = 
    length 
    . noDuplicates 
    . fmap angle 
    . delete (0,0) 
    . translate origin
    $ points

distance2 :: Point -> Integer
distance2 (x,y) = x^2 + y^2

spiralOrder :: [Point] -> [Point]
spiralOrder =
    let -- map "up" to smallest angle (instead of "right") and 
        -- increase angle clockwise instead of counter-clockwise
        angle' (x,y) = - angle (y,x)
        sortByDistance = sortBy (compare `on` distance2)
    in  concat . transpose . fmap sortByDistance . partitionBy angle'

main :: IO ()
main = do
    input <- parse parser "" <$> readFile "2019/input/10.txt"
    case input of
        Left error -> putStr (errorBundlePretty error)
        Right points -> do

            let origin = maximumBy (compare `on` (points `visibleFrom`)) points

            putStr "Part 1: "
            print $ points `visibleFrom` origin

            let (x0,y0) = origin
                (x200, y200) = (!! 199)
                    $ translate (-x0,-y0)
                    . spiralOrder
                    . translate (x0,y0)
                    . delete origin
                    $ points

            putStr "Part 2: "
            print (x200 * 100 + y200)
