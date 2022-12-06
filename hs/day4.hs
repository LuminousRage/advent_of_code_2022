import Data.List
import Data.List.Split
import Data.Ix

inputPath :: String
inputPath = "./input/input_4.txt"

parseSection :: [Char] -> [Int]
parseSection section = range (start, end)
    where   [start, end] = map read $ splitOn "-" section

isSubset :: Eq a => [a] -> [a] -> Bool
isSubset l1 l2 = intersection == l1 || intersection == l2
    where   intersection = l1 `intersect` l2

main :: IO ()
main = do
    input <- readFile inputPath
    let parsedInput = map (map parseSection . splitOn ",") $ lines input
        subsetCount = length $ filter (\[x,y] -> isSubset x y) parsedInput
        overlapCount = length $ filter (\[x,y] -> x `intersect` y /= []) parsedInput
    print subsetCount
    print overlapCount