{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use forM_" #-}
module Solver( maxMatching) where

import Control.Monad (forM)
import Data.Array    (Array, listArray, (!))
import Data.Array.IO (IOArray, newArray, readArray, writeArray)
import Data.Maybe

maxMatching :: [(Int,[Int])] -> IO (Int, [(Int,Int)])
maxMatching pairs = do
  let n   = length pairs
      adj = listArray (1,n) (map snd pairs) :: Array Int [Int]

  matchedTo <- newArray (1,n) 0 :: IO (IOArray Int Int)

  let
    findMatch :: Int -> IOArray Int Bool -> IO Bool
    findMatch u seen = go (adj ! u)
      where
        go []     = return False
        go (v:vs) = do
          isVisited <- readArray seen v
          if isVisited
            then go vs
            else do
              writeArray seen v True
              current <- readArray matchedTo v

              if current == 0
                then writeArray matchedTo v u >> return True
                else do
                  found <- findMatch current seen

                  if found
                    then writeArray matchedTo v u >> return True
                    else go vs

  _ <- forM [1..n] $ \origin -> do
    seen <- newArray (1,n) False
    findMatch origin seen

  matches <- forM [1..n] $ \target -> do
    origin <- readArray matchedTo target
    return (origin, target)

  let
    occupied = [ (o,t) | (o,t) <- matches, o /= 0 ]
    plan     = [ (home, lookupDest home) | home <- [1..n] ]
    lookupDest home = fromMaybe home (lookup home occupied)

    realMoves = length [ () | (h,d) <- plan, h /= d ]

  return (realMoves, plan)