module Structures
( Board(..),
  Coord,
  Cell,
  getBoard,
  buildBoard,
  getFixedCoord,
  getStartCoord,
  lookupMax,
  getMatrixToStr
) where

--import Data.Map((!))
import Data.Char
import qualified Data.Map as Map
import qualified Data.Set as Set  

type Coord = (Integer, Integer)
type Cell = (Integer, Bool)

data Board = Board { fixedSet :: Set.Set Integer
                   , fixedCoords :: Map.Map Integer Coord
                   , matrix :: Map.Map Coord Cell
                   } deriving (Show, Eq)


buildBoard :: [[Integer]] -> Board
buildBoard l = let matrix = buildMatrix l
                   fixedSet = buildFixedSet l
                   fixedCoords = buildFixedCoords fixedSet matrix
                in Board {fixedSet=fixedSet, fixedCoords=fixedCoords, matrix=matrix}


buildMatrix :: [[Integer]] -> Map.Map Coord Cell
buildMatrix l = let matrix = zipWith (\x r -> buildRow x r 0) l [0..] 
               in Map.fromList $ concat matrix


buildRow :: [Integer] -> Integer -> Integer -> [(Coord, Cell)]
buildRow l r c
    | l == [] = []
    | head l /= 0 = ((r, c), (head l, True)) : buildRow (tail l) r (c+1)
    | otherwise =  ((r, c), (0, False)) : buildRow (tail l) r (c+1)


buildFixedSet :: [[Integer]] -> Set.Set Integer
buildFixedSet l = Set.fromList $ concat $ map (\l' -> nonZero l') l
    where nonZero = filter (\x -> x /= 0)


buildFixedCoords :: Set.Set Integer -> Map.Map Coord Cell -> Map.Map Integer Coord
buildFixedCoords s b = Map.fromList $ reverse listMapped
    where getPairs = Map.filterWithKey (\k v -> Set.member (fst v) s) b
          mapToList = Map.toList getPairs
          listMapped = map (\(p, (n, _)) -> (n, p)) mapToList


-- Not needed using only getFixedCoord
isFixed :: Board -> Integer -> Bool
isFixed b x = Set.member x (fixedSet b)


getFixedCoord :: Board -> Integer -> Maybe Coord
getFixedCoord b x = Map.lookup x (fixedCoords b)


getStartCoord :: Board -> Coord
getStartCoord b = case Map.lookup 1 dict of Nothing -> (-1, -1)
                                            Just c -> c
    where dict = fixedCoords b


lookupMax :: (Ord k) => Map.Map k a -> k
lookupMax m = last $ Map.keys m

lookupMin :: (Ord k) => Map.Map k a -> k
lookupMin m = head $ Map.keys m


getSpace :: (Num a, Ord a) => a -> String
getSpace n
    | n < 10 = "  "
    | otherwise = " "


getMatrixToStr :: Board -> String
getMatrixToStr b = unlines $ map concat getRows
    where table    = matrix b 
          pairs    = Map.assocs table
          getRow x = map (\((_,_), (n,_)) -> show n ++ getSpace n ) $ filter (\((r,_), (_,_)) -> r == x) pairs
          top      = fst $ lookupMax table 
          getRows  = foldr (\x acc -> getRow x:acc) [] [0..top]

getBoard = [[12,0,0,24,25], [0,10,16,0,0], [0,7,1,0,0], [6,0,18,0,0], [0,0,0,0,0]]