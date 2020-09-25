{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments    #-}

module Sudoku.Internal.Bridge where
import Control.Monad (forM, mapM_)
import Data.Array.Unboxed
import Data.GI.Base
import Data.Maybe (fromJust, fromMaybe)
import Foreign.C.Types
import GI.Gtk.Objects.Button
import GI.Gtk.Objects.Grid
import GI.Gtk.Objects.Popover
import Sudoku.Internal.Sudoku
import System.Random
import Text.Read (readMaybe)
import qualified Data.Text as T


gridState :: Grid -> IO [Int]
gridState g = reverse <$> do
    children <- #getChildren g
    forM children \x -> do
        x' <- unsafeCastTo Button x
        #getLabel x' >>= \v -> if v == T.empty
            then return 0
            else return . read . T.unpack $ v


gridArray :: Grid -> IO Board
gridArray g = do
    st <- gridState g
    return . Board $ listArray ((1,1), (sudokuSz, sudokuSz)) st


gridSolve :: Grid -> IO (Maybe [Cell])
gridSolve g = do
    arr <- gridArray g
    rand <- newStdGen
    return $ solve arr rand >>= \xs -> if null xs
        then return []
        else return $ head xs


gridConflicts :: Grid -> Cell -> IO [Point]
gridConflicts g ((i, j), v) = gridArray g >>= \x ->
    return $ conflicts x ((i, j), v)

-- | Update grid by [Cell]
gridUpdate :: Grid -> [Cell] -> IO ()
gridUpdate g = mapM_ f
    where
        u 0 = T.empty
        u x = T.pack . show $ x
        f ((i, j), v) = #getChildAt g (fromIntegral j) (fromIntegral i)
            >>= \x -> do
                btn <- unsafeCastTo Button $ fromJust x
                set btn [#label := u v]


gridRegen :: Grid -> Int -> IO ()
gridRegen g dif = do
    rand <- newStdGen
    case head <$> solve clr rand of
        Nothing -> error "impossible outcome - blank grid regeneration"
        Just xs -> do
            s <- forM xs \x -> randomRIO (1, maxProb) >>= \p ->
                if dif >= p
                    then return (fst x, 0)
                    else return x
            gridUpdate g s
    where
        clr = Board $ listArray ((1,1), (sudokuSz,sudokuSz)) $ repeat 0
        maxProb = 100


keypadCell :: Grid -> Popover -> IO Cell
keypadCell g p = do
    parent <- #getRelativeTo p >>= unsafeCastTo Button
    gv <-  toGValue (0 :: CInt)
    #childGetProperty g parent "top-attach" gv
    i <- fromGValue gv :: IO CInt
    #childGetProperty g parent "left-attach" gv
    j <- fromGValue gv :: IO CInt
    v <- readInt . T.unpack <$> #getLabel parent :: IO Int
    return ((fromIntegral i, fromIntegral j), v)
    where
        readInt = fromMaybe (0 :: Int) . readMaybe
