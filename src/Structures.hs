module Structures
( 
  Board(..),
  Coord,
  Cell,
  buildBoard,
  buildFixedCoords,
  getFixedCoord,
  getStartCoord,
  lookupMax,
  getMatrixToStr,
  getStringToTable
) where

import Data.Char
import Data.List
import Text.Read
import System.Random
import qualified Data.Map as Map
import qualified Data.Set as Set  

type Coord = (Integer, Integer)
type Cell = (Integer, Bool)

data Board = Board { fixedCoords :: Map.Map Integer Coord
                   , matrix :: Map.Map Coord Cell
                   } deriving (Show, Eq)


buildBoard :: [[Integer]] -> Board
buildBoard l = let matrix      = buildMatrix l
                   fixedSet    = buildFixedSet l
                   fixedCoords = buildFixedCoords fixedSet matrix
                in Board {fixedCoords=fixedCoords, matrix=matrix}


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
    where nonZero = filter (>0)


buildFixedCoords :: Set.Set Integer -> Map.Map Coord Cell -> Map.Map Integer Coord
buildFixedCoords s b = Map.fromList $ reverse listMapped
    where getPairs   = Map.filterWithKey (\k v -> Set.member (fst v) s) b
          mapToList  = Map.toList getPairs
          listMapped = map (\(p, (n, _)) -> (n, p)) mapToList


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
    | n == -1   = "   "
    | n < 10    = "  "
    | otherwise = " "


getMatrixToStr :: Board -> String
getMatrixToStr b = unlines (map concat delRows) ++ "\n" ++ sep ++ "\n" ++ "\n"
    where table    = matrix b 
          pairs    = Map.assocs table
          getRow x = map (\((_,_), (n,_)) -> if n == -1 then getSpace n else show n ++ getSpace n ) $ filter (\((r,_), (_,_)) -> r == x) pairs
          top      = fst $ lookupMax table 
          getRows  = foldr (\x acc -> getRow x:acc) [] [0..top]
          delRows  = filter (\x -> length x /= (foldl (\acc s -> if s == "   " then acc + 1 else acc) 0 x)) getRows
          sep      = concat $ take 40 $ repeat "#"


getStringToTable :: String -> [[Integer]]
getStringToTable input
    | check     = table
    | otherwise = []
    where rawRows       = lines input
          readInteger x = getMaybeInt $ readMaybe x :: Integer
          table         = map (map readInteger) $ map words rawRows
          check         = checkTable table
          

checkTable :: [[Integer]] -> Bool
checkTable t = getMax == numCells
    where getMax    = fromInteger $ foldl (\acc x -> if maximum x > acc then maximum x else acc) 0 t
          length' x = length $ filter (-1 /=) x
          numCells  = foldl (\acc x -> acc + length' x) 0 t


getMaybeInt :: Maybe Integer -> Integer
getMaybeInt Nothing = -1
getMaybeInt (Just n) = if n >= 0 then n else 0