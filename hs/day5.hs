import Data.List.Split
import Data.Maybe
import Data.List

type Instruction = (Int, Int, Int)
type Crate = String

-- data State s a = State (s -> (a,s))
-- instance Monad (State s) where
--     return x = State $ \s -> (x, s)
    
-- updateCrate :: Int -> Crate -> ([Crate] -> (Crate, [Crate]))
-- updateCrate loc x = (\xs -> (x, (init $ take loc xs) ++ (x:drop loc xs)))

inputPath :: String
inputPath = "./input/input_5.txt"

parseCrate :: String -> Maybe Char
parseCrate crate = 
    let parsedCrate = take 3 crate in
    case parsedCrate of
        ['[', unboxedCrate, ']'] -> Just unboxedCrate
        _ -> Nothing

parseInstruction :: String -> Instruction
parseInstruction instruction = 
    let splitInstruction = splitOn " " instruction in
    case splitInstruction of
        [_, number, _, from, _, to] -> (read number, read from - 1, read to - 1)
        _ -> error "cry"

updateCrates :: [Crate] -> Int -> String -> [Crate]
updateCrates crates num newNum = front ++ (newNum:tail end)
    where   (front, end) = splitAt num crates

moveCrates :: [Crate] -> Instruction -> [Crate]
moveCrates crates (number, from, to) = update2
    where   
            fromCrates = crates !! from
            (move, newFrom) = splitAt number fromCrates
            newTo = move ++ (crates !! to)
            update1 = updateCrates crates from newFrom
            update2 = updateCrates update1 to newTo

main :: IO ()
main = do
    input <- readFile inputPath
    let [crate, instructions] = splitOn "\n\n" input
        parsedCrates = map catMaybes $ transpose $ map (map parseCrate . chunksOf 4) $ lines crate
        parsedInstructions = map parseInstruction $ lines instructions
        move = foldl moveCrates parsedCrates parsedInstructions
    print $ map head move