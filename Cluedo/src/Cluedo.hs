module Cluedo where

import System.Random
import System.Random.Shuffle
import Control.Monad
import Data.List.Split
import Data.List
import Control.Exception
--import Control.Monad.Trans.State.Strict

type Items a = [a]
type Categories a = [Items a]
type Hand a = Items a         --Hand for player
type Hands a = [Hand a]
type Cluedo a = Items a       --Secret objectives
type GameState a = (Cluedo a, Items a, Hands a)


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
splitEven lst c = if (length lst) `mod` c /= 0
  then (init cnk, last cnk)
  else (cnk, [])
  where
    dv = (length lst `div` c)
    cnk = chunksOf dv lst

hasCard :: (Eq a) => Hand a -> [a] -> Maybe a
hasCard hnd lst = find (`elem` lst) hnd

game :: (Eq a, Ord a, Read a, Show a) => GameState a -> IO ()
game gs@(cluedo, open, hands) = flip catch (\(e :: IOException) -> (putStrLn $ show e) >> game gs) $ do
  if not $ null open 
  then putStrLn "Open Cards:" >> print open
  else return ()
  putStrLn "ask or accuse? (A / C)"
  ans <- getLine
  case head ans of
    'A' -> do
      putStrLn "Player Index:"
      idx <- readLn
      putStrLn "Cards"
      lst <- readLn
      case hasCard (hands !! (idx - 1)) lst of
        Nothing -> putStrLn "Has Nothing"
        Just x -> putStrLn $ "Has " ++ show x
      game gs
    'C' -> do
      putStrLn "Accusation:"
      lst <- readLn
      if (sort lst) == (sort cluedo)
      then putStrLn "Correct! You solved the riddle!"
      else putStrLn "Wrong Accusation..." >> game gs
