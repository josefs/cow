module Maze where

import Data.Array.Unboxed

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

dyn :: LootMaze -> IOUArray (Int,Int) Int -> IO Int
dyn lootMaze arr = do
    (x,y) <- getBounds lootMaze
    loot <- readArray lootMaze (x-1, y-1)
    writeArray arr (x-1,y-1) loot
    forM_ [x-2, x-3 .. 0] $ \xp ->
      loot <- readArray lootMaze (xp,   y-1)
      acc  <- readArray arr      (xp+1, y-1)
      writeArray arr (xp, y-1) (loot + acc)
    forM_ [y-2, y-3 .. 0] $ \yp ->
      loot <- readArray lootMaze (x-1, yp)
      acc  <- readArray arr      (x-1, yp+1)
      writeArray arr (x-1, yp) (loot + acc)
    forM_ [x-1, x-2 .. 0] $ \xp ->
      forM_ [y-2, y-3 .. 0] $ \yp ->
        loot <- readArray lootMaze (xp, yp)
	acc1 <- readArray arr      (xp+1, yp)
	acc2 <- readArray arr      (xp, yp+1)
	writeArray arr (xp,yp) (loot + max acc1 acc2)
    readArray arr (0,0)
