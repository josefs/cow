module Maze where

import Data.Array.Unboxed
import Data.Array.IO
import Data.Array.MArray
import Control.Monad
import System.IO
import Data.Char

type LootMaze = UArray (Int,Int) Int

startPos = (0,0)

{- There are several ways that one could do the dynamic programming
   exploration. One is to grow a square from the end point to the start point.
   But it's complicated to keep track of the corners of the square.

   A simpler way is to just explore one column at a time (or row).
   One downside is that we need edgecases for the initial elements in each
   column (row). However, we can deal with that by initializing the first
   row (column) first, and then we get a superfast inner loop which doesn't
   need any bounds checking.
-}

readLootMaze :: String -> IO LootMaze
readLootMaze fileName = withFile fileName ReadMode $ \h ->
  do [x,y] <- fmap (fmap read .words) $ hGetLine h
     arr <- newArray_ ((0,0),(x-1,y-1)) :: IO (IOUArray (Int,Int) Int)
     forM_ [0..x-1] $ \xp ->
       forM_ [0..y-1] $ \yp ->
	 do n <- hGetNumber h
	    writeArray arr (xp,yp) n
     freeze arr

hGetNumber :: Handle -> IO Int
hGetNumber h = eraseWhiteSpace h (readNumber h)

eraseWhiteSpace :: Handle -> ([Char] -> IO r) -> IO r
eraseWhiteSpace h k = do
  c <- hGetChar h
  if isSpace c then
     eraseWhiteSpace h k
  else
     k [c]

readNumber :: Handle -> [Char] -> IO Int
readNumber h str = do
  c <- hGetChar h
  if isDigit c then
    readNumber h (c:str)
  else
    return (read (reverse str))

dyn :: LootMaze -> IOUArray (Int,Int) Int -> IO Int
dyn lootMaze arr = do
    let (_,(x,y)) = bounds lootMaze
    writeArray arr (x,y) (lootMaze ! (x, y))
    forM_ [x-1, x-2 .. 0] $ \xp -> do
      let loot = lootMaze ! (xp,   y)
      acc  <- readArray arr      (xp+1, y)
      writeArray arr (xp, y-1) (loot + acc)
    forM_ [y-1, y-2 .. 0] $ \yp -> do
      let loot = lootMaze ! (x, yp)
      acc  <- readArray arr      (x, yp+1)
      writeArray arr (x-1, yp) (loot + acc)
    forM_ [x-1, x-2 .. 0] $ \xp -> do
      forM_ [y-1, y-2 .. 0] $ \yp -> do
	let loot = lootMaze ! (xp, yp)
	acc1 <- readArray arr      (xp+1, yp)
	acc2 <- readArray arr      (xp, yp+1)
	writeArray arr (xp,yp) (loot + max acc1 acc2)
    readArray arr (0,0)
