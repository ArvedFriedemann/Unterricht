import System.Random
import Control.Monad
import Data.List

--randomIO, randomRIO

dataPoints = 1000
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
        1 -> drawChance catchChance --control group
        2 -> drawChance withMedHealChanceA
        3 -> drawChance withMedHealChanceB
        _ -> error ""
    return (i,prep,fromEnum catch, fromEnum healed)

createDB :: String -> IO ()
createDB filename = do
  db <- createData
  writeFile filename "INSERT INTO Symptoms(PersonID, Preparate, Test1, Test2) VALUES\n"
  appendFile filename $ concat $ intersperse ",\n" $ (flip map) db $ \(i,p,t1,t2) ->
    "    ("++(concat $ intersperse "," (show <$> [i,p,t1,t2]))++")"
  appendFile filename ";"


createContacts :: Int -> IO [(Int,Int)]
createContacts contactCount = (concat <$>) $ forM [1..contactCount] $ \i -> do
  contacts <- forM [1..maxdegree] (const $ randomRIO (1,contactCount))
  return $ zip (repeat i) contacts

createContactDB :: Int -> String -> IO ()
createContactDB contactCount filename = do
  db <- createContacts contactCount
  writeFile filename "INSERT INTO Contacts(PersonID, ContactID) VALUES\n"
  appendFile filename $ concat $ intersperse ",\n" $ (flip map) db $ \(i,p) ->
    "    ("++(concat $ intersperse "," (show <$> [i,p]))++")"
  appendFile filename ";"
