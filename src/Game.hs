module Game
( 
  pipeline,
  directions,
  getEmptyAdjacents,
  move
) where

import Structures
import qualified Data.Map as Map

--                N       S       E       W        NW        NE       SW      SE
directions = [(-1, 0), (1, 0), (0, -1), (0, 1), (-1, -1), (-1, 1), (1, -1), (1, 1)]


tryMove :: Board -> Coord -> Bool -> Coord -> Bool
tryMove b (k1, k2) t (d1, d2) = case Map.lookup (k1 + d1, k2 + d2) (matrix b) of Nothing -> False
                                                                                 Just c  -> not (snd c) || t


sumCoords :: Coord -> Coord -> Coord
sumCoords (x, y) (x', y') = (x + x', y + y')


getAdjacents :: Board -> Coord -> [Coord]
getAdjacents b k = map (sumCoords k) $ filter (tryMove b k True) directions


getEmptyAdjacents :: Board -> Coord -> [Coord]
getEmptyAdjacents b k = map (sumCoords k) $ filter (tryMove b k False) directions
          

isAdjacent :: Board -> Coord -> Coord -> Bool
isAdjacent b c c' = c' `elem` getAdjacents b c


move :: Board -> Integer -> Coord -> Board
move b v' k = Board {fixedCoords=(fixedCoords b), matrix=(Map.adjust (\(v, flag) -> (v', True)) k (matrix b))}


getMaybe :: Maybe a -> a
getMaybe (Just x) = x


checkSol :: Board -> Bool
checkSol b = lenTable == full
    where table     = matrix b
          full      = Map.foldl (\acc (n, _) -> if n /= 0 then acc + 1 else acc) 0 table
          lenTable  = length $ Map.assocs table


backtrack :: Board -> Coord -> Integer -> [Board]
backtrack b c x
    | fixed /= Nothing = if isAdjacent b c fixed' then backtrack b fixed' (x + 1) else []
    | not $ null empty = concatMap (\c' -> backtrack (move b x c') c' (x + 1)) empty
    | otherwise = [b]
    where fixed   = getFixedCoord b x
          fixed'  = if fixed == Nothing then (-1,-1) else getMaybe fixed
          empty   = getEmptyAdjacents b c


pipeline :: Board -> [Board]
pipeline b = filter (checkSol) sols
    where start = getStartCoord b
          sols  = backtrack b start 2