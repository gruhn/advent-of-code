module Main where

-- >>> diagonalIndex <$> [(1,1),(1,2),(2,1),(1,3),(2,2),(3,1),(1,4),(2,3),(3,2),(4,1)]
-- [0,1,2,3,4,5,6,7,8,9]

diagonalIndex :: (Int, Int) -> Int
diagonalIndex (x,y) =
    (x+y-1)*(x+y-2) `div` 2 + (x-1)

-- >>> take 5 codes
-- [20151125,31916031,18749137,16080970,21629792]

-- >>> codes !! diagonalIndex (6,6)
-- 27995004

codes :: [Integer]
codes = 
    let next code = (code * 252533) `mod` 33554393
    in  iterate next 20151125

main :: IO ()
main = do 
    let row = 2981
        column = 3075

    putStr "Part 1: "
    print $ codes !! diagonalIndex (column,row) 


