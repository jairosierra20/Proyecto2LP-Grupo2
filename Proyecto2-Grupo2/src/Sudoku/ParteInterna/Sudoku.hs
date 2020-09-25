{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}

module Sudoku.ParteInterna.Sudoku where
import Control.Monad (guard, join)
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.Function (fix)
import Data.Maybe (mapMaybe)
import Data.STRef
import Sudoku.ParteInterna.Interna
import System.Random
import qualified Data.Set as Set

newtype Tablero = Tablero (UArray Point Int)      -- +-+-+-+
                                              -- |1|2|3|
type Point = (Int, Int)                       -- +-----+
                                              -- |4|5|6|
type Cell = (Point, Int)                      -- +-----+
                                              -- |7|8|9|
sudokuSz :: Int                               -- +-+-+-+
sudokuSz = 9

blockWidth :: Int
blockWidth = 3

type ScanArea = ([Int], [Int], [Int]) -- Rows to scan, columns to scan, blocks

scanAll :: ScanArea
scanAll = let !r = [1..sudokuSz] in (r, r, r)


getBlock :: Point -> Int
getBlock (i, j) = i' + j' where
    i' = (i - 1) `div` blockWidth * blockWidth + 1
    j' = (j - 1) `div` blockWidth


blockIndices :: Int -> [Point]
blockIndices n = is `distrubute` js where
    distrubute is' js' = [ (i, j) | i <- is', j <- js' ]
    is = let del = (n - 1) `div` blockWidth  * blockWidth
        in [del + 1 .. blockWidth + del]
    js = let del = (n - 1) `mod` blockWidth  * blockWidth
        in [del + 1 .. blockWidth + del]

rowIndices :: Int -> [Point]
rowIndices i = [(i, j) | j <- [1 .. sudokuSz]]


colIndices :: Int -> [Point]
colIndices j = [(i, j) | i <- [1 .. sudokuSz]]

canidate :: [Int] -> Bool -> [(Int, [Int])]
canidate xs z = filter (\x -> (length . take 2 . snd $ x) > 1) . assocs
    $ runSTArray do
        arr <- newArray (0, sudokuSz) [] :: ST s (STArray s Int [Int])
        i <- newSTRef xs
        j <- newSTRef 1
        let appIdx d ix = do
                tmp <- readArray arr d
                writeArray arr d $ ix:tmp
        let inc to = do
                modifySTRef j (+ 1)
                modifySTRef i tail
                to
        fix $ \go -> do
            i' <- readSTRef i
            j' <- readSTRef j
            case i' of
                [] -> return ()
                (0:_) -> if z then appIdx 0 j' >> inc go else inc go
                (u:_) -> appIdx u j' >> inc go
        return arr

blockIdxCrd :: Int -> Int -> Point
blockIdxCrd b idx = (i, j) where
    (dI, dJ) = divMod (idx - 1) blockWidth
    (i', j') = let (i'', j'') = divMod (b - 1) blockWidth
               in (i'' * 3, j'' * 3)
    (i, j)   = (dI + i' + 1, dJ + j' + 1)

conflicts :: Tablero -> Cell -> [Point]
conflicts b ((i, j), x) = let
    f = flip canidate False
    (a, z, c) = unitVals b $ pointUnit (i, j) :: ([Int], [Int], [Int])
    (a', z', c') = (f a, f z, f c)
    a_ = g ((i,) <$>) a'
    z_ = g ((,j) <$>) z'
    c_ = g (blockIdxCrd (getBlock (i, j)) <$>) c'
    g u us = join $ flip mapMaybe us \(d, xs) -> if d == x && (not . null) xs
        then Just $ u xs
        else Nothing
    in  a_ <> z_ <> c_

allConflicts :: Tablero -> [Point]
allConflicts b@(Tablero g) = Set.toList $ foldr f (Set.empty :: Set.Set Point) $ assocs g where
    f x acc = let cs = Set.fromList $ conflicts b x in Set.union cs acc


cSpace :: [(Int, Int)]
cSpace = flip (,) <$> [1..sudokuSz] <*> [1..sudokuSz]

-- Posible solucion de sudoku
validateArea :: ScanArea -> Tablero -> Bool -> Bool
validateArea (row, col, blk) (Tablero g) z = null . join $ map f r <> map f c <> map f b where
    validate k xs = map (g !) . k <$> xs
    f = flip canidate z
    r = validate rowIndices row
    c = validate colIndices col
    b = validate blockIndices blk


findIndices :: Int -> Tablero -> [(Int, Int)]
findIndices n (Tablero arr) = map fst . filter (\x -> snd x == n) . assocs $ arr

fetch :: Tablero -> (Int -> [Point]) -> Int -> [Int]
fetch (Tablero g) f n = (g !) <$> f n

type Prune = (Cell -> Bool)

pruneDepends :: Cell -> Prune
pruneDepends (c, v) = \(c', v') -> if v == v'
    then fst c == fst c'
        || snd c == snd c'
        || getBlock c == getBlock c'
    else False


composePrune :: Prune -> Prune -> Prune
composePrune a b = \x -> b x || a x


forest :: [(Point, [Int])] -> [[Cell]]
forest [] = []
forest ((c', vs') : xs') = map (map fst)
    $ f xs' [[((c', v'), pruneDepends (c', v'))] | v' <- vs'] where
        f [] acc = acc
        f ((c, vs) : xs) acc = f xs [ g (c, v) u x
            | (x@((_,u):_)) <- acc, v <- vs]
        g p u xs = if u p
            then []
            else (p, composePrune (pruneDepends p) u) : xs

pointUnit :: Point -> (Int, Int, Int)
pointUnit x = (fst x, snd x, getBlock x)


unitVals :: Tablero -> (Int, Int, Int) -> ([Int], [Int], [Int])
unitVals g (a, b, c) = ( fetch g rowIndices a
                       , fetch g colIndices b
                       , fetch g blockIndices c )


missingVals :: ([Int], [Int], [Int]) -> [Int]
missingVals (a, b, c) = Set.toList $ Set.difference full some where
    full = Set.fromList [1 .. sudokuSz]
    some = Set.unions $ Set.fromList <$> [a, b, c]


solve :: Tablero -> StdGen -> Maybe [[Cell]]
solve g'@(Tablero g) rand
    | validateArea scanAll g' False = let
        is = findIndices 0 g'
        -- Shuffling the seed dec time on avg
        f a b = (a, fst $ shuffle' (missingVals b) rand)
        -- Possible solutions at each index
        rs = zipWith f is $ unitVals g' . pointUnit <$> is
        result = if null is
            then (True, [])
            else (False,)  $ do
                sltn <- forest rs :: [[Cell]]
                let test' = g  // sltn
                guard $ validateArea scanAll (Tablero test') True
                return sltn
        in case result of -- handle no result found case
            (True, x) -> Just x
            (_,  [])  -> Nothing
            (_,  xs)  -> Just xs
    | otherwise = Nothing
