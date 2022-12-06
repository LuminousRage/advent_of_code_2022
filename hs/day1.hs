import Data.List
import Data.List.Split

inputPath :: String
inputPath = "./input/input_1.txt"

readIntLines :: String -> [Integer]
readIntLines str = map readInt $ lines str
    where readInt x = read x :: Integer

main :: IO ()
main = do
    input <- readFile inputPath
    let parsedInput = map (sum . readIntLines) $ splitOn "\n\n" input
        elvesCalories = reverse $ sort parsedInput
    print $ head elvesCalories
    print $ sum $ take 3 elvesCalories