module Main where

import Cluedo
import DefaultCluedo
import Control.Exception
import Control.Monad

main :: IO _
main = do
  putStrLn "Number of players:"
  numPl <- readLn
  gs@(cluedo,open,hands) <- distribute defaultCluedo numPl
  --print gs
  showPlayer gs 0
  game gs

showPlayer :: GameState String -> Int -> IO ()
showPlayer gs@(cluedo,open,hands) idx = if idx < length hands
  then flip catch (\(e :: IOException) -> (putStrLn $ show e) >> showPlayer gs idx) $ do
    putStrLn "Show next player? (Y / N)"
    ans <- getLine
    case ans of
      "Y" -> (print $ hands !! idx) >> forM [1..50] (const $ putStrLn "\n") >> showPlayer gs (idx + 1)
      "N" -> return ()
  else putStrLn "No more players"
