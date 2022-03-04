{-# Language ViewPatterns #-}

import System.Random
import Control.Monad
import Data.List.Split


drawUnif :: IO Double
drawUnif = randomRIO (0.0, 1.0)

drawChance :: Double -> IO Bool
drawChance c = (<= c) <$> drawUnif

data DataPoint = DataPoint {
  healed :: Bool,
  tookMeds :: Bool,
  hadPrecon :: Bool
} deriving (Show)

--Table: Healed, Took Medicine, HadPreconditions
create:: Int -> IO [DataPoint]
create dp = forM [1..dp] $ \i -> do
  tookMeds <- drawChance 0.5
  hadPrecon <- drawChance 0.3
  healedChance <- return $ case (tookMeds, hadPrecon) of
    (False, False) -> 0.5
    (True, False) -> 0.9
    (False, True) -> 0.3
    (True, True) -> 0.7
  healed <- drawChance healedChance
  return $ DataPoint healed tookMeds hadPrecon

data Analysis = Analysis {
  cNoMedNoPrecon :: Float,
  cWtMedNoPrecon :: Float,
  cNoMedWtPrecon :: Float,
  cWtMedWtPrecon :: Float,
  cHealedWtMed :: Float,
  cHealedNoMed :: Float,
  medWorksGen :: Bool,
  medWorksPrecon :: Bool
} deriving (Show)

analyse :: [DataPoint] -> Analysis
analyse dat = Analysis {
  cNoMedNoPrecon = cNoMedNoPrecon,
  cWtMedNoPrecon = cWtMedNoPrecon,
  cNoMedWtPrecon = cNoMedWtPrecon,
  cWtMedWtPrecon = cWtMedWtPrecon,
  cHealedWtMed = cHealedWtMed,
  cHealedNoMed = cHealedNoMed,
  medWorksGen = cHealedWtMed > cHealedNoMed,
  medWorksPrecon = cWtMedWtPrecon > cNoMedWtPrecon
}
  where
    numof f = fromIntegral $ length $ filter f dat
    numOfGiven f g = numof (f /\ g) / numof g
    infixr 6 /\
    (/\) f g x = (f x) && (g x)

    cNoMedNoPrecon = numOfGiven healed (not . tookMeds /\ not . hadPrecon)
    cWtMedNoPrecon = numOfGiven healed (tookMeds /\ not . hadPrecon)
    cNoMedWtPrecon = numOfGiven healed (not . tookMeds /\ hadPrecon)
    cWtMedWtPrecon = numOfGiven healed (tookMeds /\ hadPrecon)
    cHealedWtMed = numOfGiven healed tookMeds
    cHealedNoMed = numOfGiven healed (not . tookMeds)

splitData :: Int -> Int -> IO [Analysis]
splitData numDat chnkSz = (map analyse) <$> chunksOf chnkSz <$> create numDat


--putStrLn =<< (unlines . map show) <$> (map (\x -> (medWorksGen x, medWorksPrecon x))) <$> splitData 60 10
--putStrLn =<< (unlines . map show) <$> (map (\x -> (cWtMedWtPrecon x, cNoMedWtPrecon x, medWorksGen x, medWorksPrecon x))) <$> splitData 60 10

--TODO: Some data has NaN!
