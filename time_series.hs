import Data.List
import qualified Data.Map as Map
import Data.Semigroup
import Data.Maybe

file1 :: [(Int,Double)]
file1 = [ (1, 200.1), (2, 199.5), (3, 199.4)
        , (4, 198.9), (5, 199.0), (6, 200.2)
        , (9, 200.3), (10, 201.2), (12, 202.9)]

file2 :: [(Int,Double)]
file2 = [(11, 201.6), (12, 201.5), (13, 201.5)
        ,(14, 203.5), (15, 204.9), (16, 207.1)
        ,(18, 210.5), (20, 208.8)]

file3 :: [(Int,Double)]
file3 = [(10, 201.2), (11, 201.6), (12, 201.5)
        ,(13, 201.5), (14, 203.5), (17, 210.5)
        ,(24, 215.1), (25, 218.7)]

file4 :: [(Int,Double)]
file4 = [(26, 219.8), (27, 220.5), (28, 223.8)
        ,(29, 222.8), (30, 223.8), (31, 221.7)
        ,(32, 222.3), (33, 220.8), (34, 219.4)
        ,(35, 220.1), (36, 220.6)]

-- Step 1 -> make a basic type for your time series --
-- After create a func that takes a list of values and creates a TS type --

data TS a = TS [Int] [Maybe a]

createTS :: [Int] -> [a] -> TS a
createTS times values = TS completeTimes extendedValues
   where
    completeTimes = [minimum times .. maximum times]
    timeValueMap = Map.fromList (zip times values)
    extendedValues = map (\v-> Map.lookup v timeValueMap) completeTimes

-- Step 2 -> Create a helper function to "format your files" (unzip them) --
-- create a way to show your time value pairs --
-- Create an instance of show using both of these functions --

fileToTS :: [(Int,a)] -> TS a
fileToTS tvPairs = createTS times values
   where
       (times,values) = unzip tvPairs

showTVPair :: Show a => Int -> (Maybe a) -> String
showTVPair time (Just value) = mconcat [show time, "|" , show value, "\n"]
showTVPair time Nothing = mconcat [show time ,"|NA\n"]

instance Show a => Show (TS a) where
    show (TS times values) = mconcat rows
       where
           rows = zipWith showTVPair times values

-- Step 3 -> Convert all your "Files" to TS types --

ts1 :: TS Double
ts1 = fileToTS file1
ts2 :: TS Double
ts2 = fileToTS file2
ts3 :: TS Double
ts3 = fileToTS file3
ts4 :: TS Double
ts4 = fileToTS file4

-- Step 4 -> Combining TS types --

insertMaybePair :: Ord k => Map.Map k v -> (k, Maybe v) -> Map.Map k v
insertMaybePair myMap (_,Nothing) = myMap
insertMaybePair myMap (key,(Just value)) = Map.insert key value myMap

combineTSTypes :: TS a -> TS a -> TS a
combineTSTypes (TS [] []) ts2 = ts2
combineTSTypes ts1 (TS [] []) = ts1
combineTSTypes (TS t1 v1)(TS t2 v2) = TS completeTimes combinedValues
    where
        bothTimes = mconcat [t1,t2]
        tvMap = foldl insertMaybePair Map.empty (zip t1 v1)
        updatedMap = foldl insertMaybePair tvMap (zip t2 v2)
        completeTimes = [minimum bothTimes .. maximum bothTimes]
        combinedValues = map (\v -> Map.lookup v updatedMap) completeTimes