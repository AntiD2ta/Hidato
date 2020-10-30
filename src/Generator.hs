module Generator
( getRandomBoard
) where

import Game
import Structures
import System.Random
import qualified Data.Map as Map
import qualified Data.Set as Set 


buildRectangle :: Int -> Int -> [[Integer]]
buildRectangle x y = take x $ repeat $ take y $ repeat (0 :: Integer)


genCoord :: Integer -> StdGen -> (Coord, StdGen)
genCoord top gen = ((x, y), gen'')
    where (x, gen')  = randomR (0, top) gen
          (y, gen'') = randomR (0, top) gen'


getRandomBoard :: StdGen -> (Board, StdGen)
getRandomBoard gen = (filterBoard genBoard, gen'')
    where (top, gen')    = randomR (1, 20) gen
          top'           = toInteger top
          (start, gen'') = genCoord top' gen'
          table          = buildRectangle top top
          rawBoard       = buildBoard table
          genBoard       = walkRandom gen'' start 1 rawBoard


walkRandom :: StdGen -> Coord -> Integer -> Board -> Board
walkRandom gen c x b
    | null empty = move b (x - 1) c
    | erase      = walkRandom gen'' c' (x + 1) $ move b (-2) c'
    | otherwise  = walkRandom gen'' c' (x + 1) $ move b x c'
    where empty           = getEmptyAdjacents b c
          (c', gen')      = randomDir gen empty
          (erase', gen'') = uniform gen' 60
          erase           = erase' && x /= 1
          

randomDir :: StdGen -> [Coord] -> (Coord, StdGen)
randomDir gen [] = ((-1,-1), gen)
randomDir gen l  = (dir, newGen)
    where len           = length l
          (idx, newGen) = randomR (0, len - 1) gen
          dir           = l !! idx


filterBoard :: Board -> Board
filterBoard b = Board {fixedCoords=(fixedCoords), matrix=(cleanErasedNums)}
    where cleanZeros       = Map.map (\(v, flag) -> if v == 0 then (-1, True) else (v, flag)) (matrix b)
          cleanErasedNums  = Map.map (\(v, flag) -> if v == -2 then (0, False) else (v, flag)) (cleanZeros)
          fixedSet         = Set.fromList $ filter (0<) $ map (\v -> fst v) $ Map.elems cleanErasedNums
          fixedCoords      = buildFixedCoords fixedSet cleanErasedNums
          

uniform :: StdGen -> Integer -> (Bool, StdGen)
uniform gen d = (u <= d, newGen)
    where (u, newGen) = randomR (0, 100) gen