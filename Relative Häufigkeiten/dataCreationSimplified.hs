{-# Language ViewPatterns #-}

import System.Random
import Control.Monad
import Data.List.Split


drawUnif :: IO Double
drawUnif = randomRIO (0.0, 1.0)

drawChance :: Double -> IO Bool
drawChance c = (<= c) <$> drawUnif

data DataPoint = DataPoint {
  personId :: Int,
  healed :: Bool,
  tookMeds :: Bool
} deriving (Show)

--Table: Healed, Took Medicine, HadPreconditions
create:: Int -> IO [DataPoint]
create dp = forM [1..dp] $ \i -> do
  tookMeds <- drawChance 0.5
  hadPrecon <- drawChance 0.3
  healedChance <- return $ case tookMeds of
    True -> 0.8
    False -> 0.5
  healed <- drawChance healedChance
  return $ DataPoint (i) healed tookMeds

data Analysis = Analysis {
  cHealedWtMed :: Float,
  cHealedNoMed :: Float,
  medWorksGen :: Bool
} deriving (Show)

analyse :: [DataPoint] -> Analysis
analyse dat = Analysis {
  cHealedWtMed = cHealedWtMed,
  cHealedNoMed = cHealedNoMed,
  medWorksGen = cHealedWtMed > cHealedNoMed
}
  where
    numof f = fromIntegral $ length $ filter f dat
    numOfGiven f g = numof (f /\ g) / numof g
    infixr 6 /\
    (/\) f g x = (f x) && (g x)

    cHealedWtMed = numOfGiven healed tookMeds
    cHealedNoMed = numOfGiven healed (not . tookMeds)

splitData :: Int -> Int -> Int -> IO [(Analysis,[DataPoint])]
splitData nonworks numDat chnkSz = do
  dat <- chunksOf chnkSz <$> create numDat
  let res = map analyse dat
  if goodAnalysis nonworks res then return (zip res dat) else splitData nonworks numDat chnkSz

goodAnalysis :: Int -> [Analysis] -> Bool
goodAnalysis nonworks ana = (nonworks == (length . filter (not . medWorksGen) $ ana)) &&
                    (all ( (/= (1/0)) . cHealedWtMed) ana) &&
                    (all ( (/= (1/0)) . cHealedWtMed) ana)

mkTeXTable :: (Analysis, [DataPoint]) -> String
mkTeXTable (ana, dat) = "\\begin{tabular}{l|l|l}\n\\hline\n\\multicolumn{1}{|l|}{\\begin{tabular}[c]{@{}l@{}}Personen-\\\\ kennzahl\\end{tabular}} & \\begin{tabular}[c]{@{}l@{}}Medikament\\\\ genommen?\\end{tabular} & \\multicolumn{1}{l|}{Geheilt} \\\\ \\hline"
  ++concat [ show i ++ " & " ++ bts tookMeds ++ " & " ++ bts healed ++ " \\\\\n"| DataPoint i healed tookMeds <- dat]
  ++"\\end{tabular}\\\\\n" ++"%"++show ana++"\n"
  where bts x = if x then "Ja" else "Nein"

mkTeXData :: Int -> Int -> Int -> IO String
mkTeXData nonworks numDat chnkSz = do
  dat <- splitData nonworks numDat chnkSz
  let lst = map mkTeXTable dat
  return $ concat ["\\subsection*{Gruppe "++show i++"}\n" ++ tab | (tab,i) <- zip lst [1..]]

--putStrLn =<< (unlines . map show) <$> (map (\x -> (medWorksGen x, medWorksPrecon x))) <$> splitData 2 60 10
--putStrLn =<< (unlines . map show) <$> splitData 2 60 10
--mkTeXData 2 70 10 >>= putStrLn



--TODO: Some data has NaN!
