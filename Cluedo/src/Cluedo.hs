module Cluedo where

import System.Random
import System.Random.Shuffle
import Control.Monad
import Data.List.Split
--import Control.Monad.Trans.State.Strict

type Items a = [a]
type Categories a = [Items a]
type Hand a = Items a         --Hand for player
type Hands a = [Hand a]
type Cluedo a = Items a       --Secret objectives

--distirbutes the cluedo, leftover cards and players hands
distribute :: Categories a -> Int -> IO (Cluedo a, Items a, Hands a)
distribute cats playerCount = do
  catsShuffle <- mapM shuffleM cats
  let
    clue = map head catsShuffle
    rest = map tail catsShuffle
    wholeStack = concat rest
  shuffleStack <- shuffleM wholeStack
  let
    (hands, open) = splitEven shuffleStack playerCount
  return (clue, open, hands)

splitEven :: [a] -> Int -> ([[a]], [a])
splitEven lst c = (init cnk, last cnk)
  where cnk = chunksOf (c `div` length lst) lst
