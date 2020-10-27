module Game
( pipeline
) where

import Structures
import qualified Data.Map as Map

--                N       S       E       W        NW        NE       SW      SE
directions = [(-1, 0), (1, 0), (0, -1), (0, 1), (-1, -1), (-1, 1), (1, -1), (1, 1)]


tryMove :: Board -> Coord -> Bool -> Coord -> Bool
tryMove b (k1, k2) t (d1, d2) = case Map.lookup (k1 + d1, k2 + d2) (matrix b) of Nothing -> False
                                                                                 Just c  -> not (snd c) || t

getAdjacents :: Board -> Coord -> [Coord]
getAdjacents b k = filter (tryMove b k True) directions


getEmptyAdjacents :: Board -> Coord -> [Coord]
getEmptyAdjacents b k = map sumCoords $ filter (tryMove b k False) directions
    where x = fst k
          y = snd k
          sumCoords (x', y') = (x + x', y + y')


isAdjacent :: Board -> Coord -> Coord -> Bool
isAdjacent b c c' = c' `elem` getAdjacents b c


move :: Board -> Integer -> Coord -> Board
move b v' k = Board {fixedSet=(fixedSet b), fixedCoords=(fixedCoords b), matrix=(Map.adjust (\(v, flag) -> (v', True)) k (matrix b))}


getMaybe :: Maybe a -> a
getMaybe (Just x) = x


getSol :: Integer -> [[Board]] -> [Board]
getSol _ [] = []
getSol n (x:xs)
    | checkSol x n = x
    | otherwise = getSol n xs


checkSol :: [Board] -> Integer -> Bool
checkSol [] _ = False
checkSol (b:bs) x = maxCell == x
    where maxCell = lookupMax (fixedCoords b)


backtrack :: Board -> Coord -> Integer -> [Board]
backtrack b c x
    | fixed /= Nothing = if isAdjacent b c fixed' then backtrack (move b x fixed') fixed' (x + 1) else [b]
    | not $ null empty = getSol x $ map (\c' -> backtrack (move b x c') c' (x + 1)) empty
    | otherwise = [b]
    where fixed   = getFixedCoord b x
          fixed'  = if fixed == Nothing then (-1,-1) else getMaybe fixed
          empty   = getEmptyAdjacents b c


pipeline :: [Board]
pipeline = backtrack b start 2
    where start = getStartCoord b
          b = buildBoard getBoard


compareWithHead :: (Eq a) => a -> [a] -> Bool
compareWithHead _ [] = False
compareWithHead y (x:xs) = x /= y