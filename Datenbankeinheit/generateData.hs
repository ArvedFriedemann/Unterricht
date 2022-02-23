{-# LANGUAGE ViewPatterns #-}
import System.Random
import Control.Monad
import Data.List

--randomIO, randomRIO

dataPoints = 250
maxdegree = 5

preparates = 3
catchChance = 0.7
noMedHealChance = 0.3
withMedHealChanceA = noMedHealChance
withMedHealChanceB = noMedHealChance*2

drawUnif :: IO Double
drawUnif = randomRIO (0.0, 1.0)

drawChance :: Double -> IO Bool
drawChance c = (<= c) <$> drawUnif

--Table: PersonID, Preparate, Test1, Test2
createData :: IO [(Int,Int,Int,Int)]
createData = do
  forM [1..dataPoints] $ \i -> do
    catch <- drawChance catchChance
    prep <- randomRIO (1,preparates)
    healed <- case catch of
      False -> return False
      True -> case prep of
        1 -> drawChance noMedHealChance --control group
        2 -> drawChance withMedHealChanceA
        3 -> drawChance withMedHealChanceB
        _ -> error ""
    return (i,prep,fromEnum catch, fromEnum $ not healed)

createDB :: String -> IO ()
createDB filename = do
  db <- createData
  writeFile filename "INSERT INTO Symptoms(PersonID, Preparate, Test1, Test2) VALUES\n"
  appendFile filename $ concat $ intersperse ",\n" $ (flip map) db $ \(i,p,t1,t2) ->
    "    ("++(concat $ intersperse "," (show <$> [i,p,t1,t2]))++")"
  appendFile filename ";"


createContacts :: Int -> IO [(Int,Int)]
createContacts contactCount = (concat <$>) $ forM [1..contactCount] $ \i -> do
  (nub -> contacts) <- forM [1..maxdegree] (const $ randomRIO (1,contactCount))
  return $ zip (repeat i) contacts

createContactDB :: Int -> String -> IO ()
createContactDB contactCount filename = do
  db <- createContacts contactCount
  writeFile filename "INSERT INTO Contacts(PersonID, ContactID) VALUES\n"
  appendFile filename $ concat $ intersperse ",\n" $ (flip map) db $ \(i,p) ->
    "    ("++(concat $ intersperse "," (show <$> [i,p]))++")"
  appendFile filename ";"


createLongProduct :: Int -> String
createLongProduct n = "SELECT count(DISTINCT "++c n++".ContactID) FROM "++(concat $ intersperse ", " [" Contacts as "++c i | i <- [1..n]])++" WHERE "++(concat $ intersperse " and " $ for (zip [1..n-1] [2..n]) $ \(i1,i2) -> c i1++".ContactID="++c i2++".PersonID")
  where c x = "c"++show x

createLongJoin :: Int -> String
createLongJoin n = "SELECT count(DISTINCT "++c n++".ContactID) FROM "++(concat $ intersperse " JOIN " [" Contacts as "++c i | i <- [1..n]])++" ON "++(concat $ intersperse " and " $ for (zip [1..n-1] [2..n]) $ \(i1,i2) -> c i1++".ContactID="++c i2++".PersonID")
  where c x = "c"++show x

createLongJoin2 :: Int -> String
createLongJoin2 n = "SELECT count(DISTINCT "++c n++".ContactID) FROM "++(recursiveJoin (zip [1..n-1] [2..n]) )
  where
    c x = "c"++show x
    recursiveJoin :: [(Int,Int)] -> String
    recursiveJoin [] = ""
    recursiveJoin [(i1,i2)] = "Contacts as "++c i1++" JOIN Contacts "++ c i2 ++ " ON "++c i1++".ContactID="++c i2++".PersonID"
    recursiveJoin ((i1,i2) : xs) = "Contacts as "++c i1++" JOIN ("++ recursiveJoin xs ++ ") as "++c i2++" ON "++c i1++".ContactID="++c i2++".PersonID"

for :: [a] -> (a -> b) -> [b]
for = flip map
