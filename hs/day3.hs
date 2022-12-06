import Data.List
import Data.Char

inputPath :: String
inputPath = "./input/input_3.txt"

splitCompartments :: String -> (String, String)
splitCompartments rucksack = splitAt halfList rucksack
    where halfList = length rucksack `div` 2

commonItem :: String -> String -> String
commonItem r1 r2 = nub $ intersect r1 r2

priority :: Char -> Int
priority item
    | item >= 'a' = ord item - 96
    | otherwise = ord item - 38

findCommon :: [String] -> [String]
findCommon [a, b, c] =  [commonItem c $ commonItem a b]
findCommon lst = findCommon (take 3 lst) ++ findCommon (drop 3 lst)

main :: IO Int
main = do
    input <- readFile inputPath
    let parsedInput = lines input
        compartments = map splitCompartments parsedInput
        itemPriority = map (\(r1,r2) -> sum $ map priority (commonItem r1 r2)) compartments
        common = map priority $ concat $ findCommon parsedInput
    return $ sum common