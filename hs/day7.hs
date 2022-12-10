
import Data.List

inputPath :: String
inputPath = "./input/input_7.txt"

data Dir a = Leaf Int | Branch String [Dir a] deriving Show

nextDir :: Int -> [[String]] -> [[String]]
nextDir _ [] = []
nextDir 0 x = x
nextDir num (x:xs) =
    case x of
        ["$", "cd", ".."] -> nextDir (num-1) xs
        ["$", "cd", _] -> nextDir (num+1) xs
        _ -> nextDir num xs

buildDir :: [[String]] -> Dir a
buildDir (x:xs) =
    case x of
        ["$", "cd", name] -> Branch name $ buildContent (tail xs)

buildContent :: [[String]] -> [Dir a]
buildContent [] = []
buildContent (x:xs) =
    case x of
        ["$", "cd", ".."] -> []
        ["$", "cd", name] -> buildDir (x:xs):buildContent next
        ["$", _] -> buildContent xs
        ["dir", _] -> buildContent xs
        [size, _] -> Leaf (read size):buildContent xs
    where next = nextDir 1 xs

getSize :: Dir a -> Int
getSize (Leaf x) = x
getSize (Branch _ (x:xs)) = sum $ map getSize (x:xs)

allDirSize :: [Dir a] -> [Int]
allDirSize [] = []
allDirSize (x:xs) =
    case x of
        Leaf _ -> allDirSize xs
        penis@(Branch _ dirs) -> (getSize penis:allDirSize dirs) ++ allDirSize xs

main :: IO ()
main = do
    input <- readFile inputPath
    let parsedInput = map words $ lines input
        rootDir = buildDir parsedInput
        rootSize = getSize rootDir
        dirSizes = allDirSize [rootDir]
    print $ sum $ filter (<= 100000) dirSizes
    print $ minimum $ filter (>= (30000000-70000000+rootSize)) dirSizes
    