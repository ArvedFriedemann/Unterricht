import System.Random
import Control.Monad
import Data.List


drawUnif :: IO Double
drawUnif = randomRIO (0.0, 1.0)

drawChance :: Double -> IO Bool
drawChance c = (<= c) <$> drawUnif

--needs to be at least length 4 to work
createData :: Int -> IO [(Int, Bool)]
createData n = do
  chance <- randomRIO (0.2,0.8)
  forM [1..n] $ \i -> do
    v <- drawChance chance
    return (i, v)

data Analysis = Analysis {
  initData :: [(Int,Bool)],
  restData :: [(Int,Bool)],
  choice :: Bool,
  wasRight :: Bool
} deriving Show

analyse :: [(Int,Bool)] -> IO Analysis
analyse dat = do
  split <- randomRIO (2, (length dat) - 2)
  let (initData, restData) = splitAt split dat
  let choice = (length $ (filter snd initData)) > (length $ (filter snd restData))
  let wasRight = (length $ map ((== choice) . snd) dat) >= (length $ map ((/= choice) . snd) dat)
  return $ Analysis initData restData choice wasRight

createAnalysis :: Int -> IO Analysis
createAnalysis n
  | n <= 4 = error "n too small"
  | otherwise = createData n >>= analyse

prettyPrintAnalysis :: Analysis -> (String, String, String)
prettyPrintAnalysis a@(Analysis initData restData choice wasRight) = (initAna, postAna, appendix)
  where
    initAna = dataToTable initData
    postAna = dataToTable restData
    appendix = "%"++(show a)++"\n"

dataToTable :: [(Int,Bool)] -> String
dataToTable dat = "\\begin{tabular}{|c|"++(concat $ replicate (length dat) "c|")++"}\n\\hline\n" ++
  "Mission: & "++ (concat $ intersperse " & " (map (show . fst) dat)) ++"\\\\\n"++
  "\\hline\n"++
  "Werkzeug: & "++ (concat $ intersperse " & " (map ((\x -> if x then "A" else "B") . snd) dat)) ++"\\\\\n"++
  "\\hline\n"++
  "\\end{tabular}\n"

--(prettyPrintAnalysis <$> (createAnalysis 10)) >>= \(x,y,z) -> putStrLn x >> putStrLn y >> putStrLn z

makeAppendix :: Int -> Int -> IO String
makeAppendix misSize groups = do
  tabs <- forM [1..groups] $ const $ prettyPrintAnalysis <$> createAnalysis misSize
  return $
    "\\newpage\n\\section*{Anhang I}\n"++ concat ["\\subsection*{Gruppe "++show g++"}\n"++tab1++appendix++"\n\n" | ((tab1,_,appendix), g) <- (zip tabs [1..groups])] ++
    "\\newpage\n\\section*{Anhang II}\n"++ concat ["\\subsection*{Gruppe "++show g++"}\n"++tab2++appendix++"\n\n" | ((_,tab2,appendix), g) <- (zip tabs [1..groups])]

--makeAppendix 15 9 >>= putStrLn
