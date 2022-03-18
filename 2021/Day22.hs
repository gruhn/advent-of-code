module Day22 where
import Text.Parsec.String (Parser, parseFromFile)
import Text.Parsec (string, many1, (<|>), newline, sepBy, char, option, try)
import Text.Parsec.Char (digit)

type Range = (Int, Int)

data Cuboid = 
    Cuboid Range Range Range
    deriving Show

intP :: Parser Int
intP = do 
    sign <- option "" (string "-")
    digits <- many1 digit
    return (read $ sign ++ digits)

rangeP :: Parser Range
rangeP = do
    start <- intP
    string ".."
    end <- intP
    return (start, end)

cuboidStateP :: Parser Bool
cuboidStateP = try on <|> off where
    on = True <$ string "on"
    off = False <$ string "off"

cuboidP :: Parser (Bool, Cuboid)
cuboidP = do
    state  <- cuboidStateP
    xRange <- string " x=" *> rangeP
    yRange <- string ",y=" *> rangeP
    zRange <- string ",z=" *> rangeP
    return (state, Cuboid xRange yRange zRange)

insideInitArea :: Cuboid -> Bool
insideInitArea (Cuboid (x0,xn) (y0,yn) (z0,zn)) =
    all (-50 <=) [ x0, y0, z0 ] && 
    all (<= 50) [ xn, yn, zn ]

day22 :: IO ()
day22 = do
    instructions <- parseFromFile (cuboidP `sepBy` newline) "22-input.txt"
    let resultVolume = sum . map volume . foldl applyInst []
    putStr "Part 1: "
    print $ resultVolume . filter (insideInitArea . snd) <$> instructions
    putStr "Part 2: "
    print $ resultVolume <$> instructions

volume :: Cuboid -> Int
volume (Cuboid (x0,xn) (y0,yn) (z0,zn)) =
    let xl = xn-x0+1  
        yl = yn-y0+1 
        zl = zn-z0+1
        sign = signum $ minimum [ xl, yl, zl ]
    in sign * abs (xl*yl*zl)

applyInst :: [Cuboid] -> (Bool, Cuboid) -> [Cuboid]
applyInst cs (False, c) = concatMap (`difference` c) cs
applyInst cs (True, c) = c : concatMap (`difference` c) cs 

data Axis = X Int | Y Int | Z Int

slice :: Axis -> Cuboid -> (Cuboid, Cuboid)
slice (X axis) (Cuboid (x0, xn) yr zr) =
    ( Cuboid (x0, min (axis-1) xn) yr zr
    , Cuboid (max x0 axis,xn) yr zr
    )
slice (Y axis) (Cuboid xr (y0,yn) zr) =
    ( Cuboid xr (y0, min (axis-1) yn) zr
    , Cuboid xr (max y0 axis,yn) zr
    )
slice (Z axis) (Cuboid xr yr (z0,zn)) =
    ( Cuboid xr yr (z0, min (axis-1) zn)
    , Cuboid xr yr (max z0 axis, zn)
    )

difference :: Cuboid -> Cuboid -> [Cuboid]
difference c0 (Cuboid (x0,xn) (y0,yn) (z0,zn)) = 
    let (west, c1) = slice (X x0) c0
        (c2, east) = slice (X (xn+1)) c1
        (south, c3) = slice (Y y0) c2
        (c4, north) = slice (Y (yn+1)) c3
        (below, c5) = slice (Z z0) c4
        (c6, above) = slice (Z (zn+1)) c5
    in filter ((> 0) . volume)
        [ west, east, north, south, above, below ] 