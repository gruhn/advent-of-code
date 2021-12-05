module ParseUtil where
import Data.List (stripPrefix)

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn sep list = aux list []
    where  
        aux [] segment = [reverse segment]
        aux (a:as) segment = 
            case stripPrefix sep (a:as) of
                Just suffix -> reverse segment : aux suffix []
                Nothing     -> aux as (a:segment)