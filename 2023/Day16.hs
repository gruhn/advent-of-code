module Main where
import Utils (withCoords)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad (guard)
import Data.Maybe (maybeToList)

type Pos = (Int,Int)

parse :: String -> Map Pos Char
parse = Map.fromList . withCoords . lines

data Dir = Lft | Rgt | Up | Dwn
  deriving (Show, Eq, Ord)

step :: (Pos, Dir) -> (Pos, Dir)
step ((x,y), dir) = (,dir) $
  case dir of
    Up  -> (x,y-1)
    Dwn -> (x,y+1)
    Lft -> (x-1,y)
    Rgt -> (x+1,y)

type BeamSet = Set (Pos, Dir)

advanceBeams :: Map Pos Char -> BeamSet -> BeamSet -> BeamSet
advanceBeams grid beams energized 
  | null beams = energized
  | otherwise  = advanceBeams grid beams_advanced (energized <> beams)
  where
    beams_advanced :: BeamSet
    beams_advanced = Set.fromList $ do
      beam <- Set.toList beams
      new_beam <- advance beam
      guard $ fst new_beam `Map.member` grid
      guard $ new_beam `Set.notMember` energized
      return new_beam

    advance :: (Pos, Dir) -> [(Pos, Dir)]
    advance (pos, dir) = do
      cell <- maybeToList $ Map.lookup pos grid
      case (cell, dir) of
        ('.', _)    -> [ step (pos, dir) ]

        ('|', Up)   -> [ step (pos, dir) ] 
        ('|', Dwn)  -> [ step (pos, dir) ] 
        ('|', Lft)  -> [ step (pos, Up), step (pos, Dwn) ]
        ('|', Rgt)  -> [ step (pos, Up), step (pos, Dwn) ]

        ('-', Lft)  -> [ step (pos, dir) ] 
        ('-', Rgt)  -> [ step (pos, dir) ] 
        ('-', Up)   -> [ step (pos, Rgt), step (pos, Lft) ]
        ('-', Dwn)  -> [ step (pos, Rgt), step (pos, Lft) ]

        ('/', Rgt)  -> [ step (pos, Up) ]
        ('/', Dwn)  -> [ step (pos, Lft) ]
        ('/', Lft)  -> [ step (pos, Dwn) ]
        ('/', Up)   -> [ step (pos, Rgt) ]

        ('\\', Rgt) -> [ step (pos, Dwn) ]
        ('\\', Lft) -> [ step (pos, Up) ]
        ('\\', Up)  -> [ step (pos, Lft) ]
        ('\\', Dwn) -> [ step (pos, Rgt) ]

        (_, _) -> undefined

collectEnergized :: Map Pos Char -> Pos -> Dir -> Set Pos
collectEnergized grid start_pos start_dir = 
  Set.map fst $ advanceBeams grid (Set.singleton (start_pos, start_dir)) Set.empty 

main :: IO ()
main = do
  grid <- parse <$> readFile "input/16.txt"

  putStr "Part 1: "
  print $ Set.size $ collectEnergized grid (0,0) Rgt

  let (max_x, max_y) = maximum $ Map.keys grid
      top_row = [ ((x,0), Dwn) | x <- [0..max_x] ]
      bot_row = [ ((x,max_y), Up) | x <- [0..max_x] ]
      lft_col = [ ((0,y), Rgt) | y <- [0..max_y] ]
      rgt_col = [ ((max_x,y), Lft) | y <- [0..max_y] ]

  putStr "Part 2: "
  print $ maximum $ do
    (start_pos, start_dir) <- top_row ++ bot_row ++ lft_col ++ rgt_col
    return $ Set.size $ collectEnergized grid start_pos start_dir
