module Cluedo where

import System.Random
import System.Random.Shuffle
import Control.Monad
import Data.List.Split
import Data.List
import Control.Exception
import Debug.Trace
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

game :: Categories String -> GameState String -> IO ()
game cats gs@(cluedo, open, hands) = flip catch (\(e :: IOException) -> (putStrLn $ show e) >> game cats gs) $ do
  if not $ null open
  then putStrLn "Open Cards:" >> print open
  else return ()
  putStrLn "ask or accuse? (A / C)"
  ans <- getLine
  case head (ans ++ " ") of
    'A' -> do
      putStrLn "Player Index:"
      idx <- readLn
      putStrLn "Cards"
      printCats cats
      let
        readStuff = do
          lst <- readLn
          if (or [(r <= 0 || length cats < r) ||
                    (c <=0 || length (cats !! (r - 1)) < c) | (r,c) <- lst])
          then putStrLn "Indices don't fit!" >> readStuff
          else return lst
      lst <- readStuff
      let choice = [(cats !! (r - 1)) !! (c - 1) | (r,c) <- lst]
      putStrLn $ "Asking: "++show choice
      case hasCard (hands !! (idx - 1)) choice of
        Nothing -> putStrLn "Has Nothing"
        Just x -> putStrLn $ "Has " ++ show x
      game cats gs
    'C' -> do
      putStrLn "Accusation:"
      lst <- readLn
      if (sort lst) == (sort cluedo)
      then putStrLn "Correct! You solved the riddle!"
      else putStrLn "Wrong Accusation..." >> game cats gs
    _ -> game cats gs

printCats :: Categories String -> IO ()
printCats cats = forM_ (zip cats [1..]) $ \(c,idx) -> do
  putStr $ show idx ++": "
  print $ map (\(i,c') -> "("++show i++": "++c'++") ") (zip [1..] c)
