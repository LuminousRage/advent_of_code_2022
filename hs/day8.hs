import Data.List

inputPath :: String
inputPath = "./input/input_8.txt"

-- for part 1: calculate whether or not visible from one side
sideVis :: [Int] -> Bool
sideVis [x] = True
sideVis (x:xs) = x > maximum xs

-- for part 2: calculate score from one side
sideScore :: [Int] -> Int
sideScore [] = 0
sideScore (x:xs)
    | numShorter == length xs = numShorter -- no block
    | otherwise = numShorter + 1 -- include blocking tree
    where numShorter = length $ takeWhile (< x) xs

-- do operation from the right
checkSide :: ([Int] -> a) -> [Int] -> [a]
checkSide f [] = []
checkSide f lst@(x:xs) = f lst:checkSide f xs

-- runs checkSide on both right and left
-- g function:
-- for part 1: we want to zipWith || (check if visible from any side)
-- for part 2: we want to zipWith * (multiply both scenic score)
checkRow :: ([Int] -> a) -> (a -> a -> a) -> [Int] -> [a]
checkRow f g row = zipWith g reversedCheckedRow $ checkSide f row
    where reversedCheckedRow = reverse $ checkSide f $ reverse row

-- runs checkRow both vertically and horizontally
-- f is the function used in checkSide
-- g is the function used in checkRow
checkTree :: ([Int] -> a) -> (a -> a -> a) -> [[Int]] -> [[a]]
checkTree f g trees = zipWith (zipWith g) vertical horizontal
    where   vertical = map (checkRow f g) trees
            transposedInput = transpose trees
            horizontal = transpose $ map (checkRow f g) transposedInput

main :: IO ()
main = do
    input <- lines <$> readFile inputPath
    let parsedInput = map (map (\x -> read [x] :: Int)) input
        visible = checkTree sideVis (||) parsedInput
        score = checkTree sideScore (*) parsedInput
    print $ length $ filter (==True) $ concat visible
    print $ maximum $ concat score