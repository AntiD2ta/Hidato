module Structures
( Board
) where

import qualified Data.Map as Map

type Coord = (Integer, Integer)
type Cell = (Integer, Bool)

data Board = Board { startCell :: Coord
                   , endCell :: Coord
                   , matrix :: Map.Map Coord Cell
                   } deriving (Show)

buildBoard :: [[Integer]] -> Board
buildBoard l = let matrix = buildMatrix l
                   start = getCell 1 matrix
                   ((_,_), (max,_)) = getMax matrix
                   end = getCell max matrix 
                in Board {startCell=start, endCell=end, matrix=matrix}
    where getCell target = Map.foldlWithKey (\acc k v -> if fst v == target  then k else acc) (0,0)
          getMax = Map.foldlWithKey (\acc k v -> if fst v > fst (snd acc) then (k, v) else acc) ((0,0), (-1, False))

buildMatrix :: [[Integer]] -> Map.Map Coord Cell
buildMatrix l = let matrix = zipWith (\x r -> buildRow x r 0) l [0..] 
               in Map.fromList $ concat matrix

buildRow :: [Integer] -> Integer -> Integer -> [(Coord, Cell)]
buildRow l r c
    | l == [] = []
    | head l /= 0 = ((r, c), (head l, True)) : buildRow (tail l) r (c+1)
    | otherwise =  ((r, c), (0, False)) : buildRow (tail l) r (c+1)

getStart m = Map.foldlWithKey (\acc k v -> if fst v == 1 then k else acc) (0,0) m

getBoard = [[12,0,0,24,25], [0,10,16,0,0], [0,7,1,0,0], [6,0,18,0,0], [0,0,0,0,0]]