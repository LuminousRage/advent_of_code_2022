-- todo: clean this up
import Data.List

inputPath :: String
inputPath = "./input/input_2.txt"

data Shape = Rock | Paper | Scissor deriving (Eq, Enum, Show)
data Outcome = Win | Draw | Loss deriving (Show)

shapePoint :: Shape -> Int
shapePoint = (+1) . fromEnum

outcomePoints :: Outcome -> Int
outcomePoints Win = 6
outcomePoints Draw = 3
outcomePoints Loss = 0

outcome :: Shape -> Shape -> Outcome
outcome Rock Paper = Win
outcome Paper Scissor = Win
outcome Scissor Rock = Win
outcome opponent you
    | opponent == you = Draw
    | otherwise = Loss

roundPoints :: [Shape] -> Int
roundPoints [opponent, you] = (+ shapePoint you) . outcomePoints $ outcome opponent you

parseShape :: Char -> Shape
parseShape shape = 
    case shape of
        'A' -> Rock
        'B' -> Paper
        'C' -> Scissor
        'X' -> Rock
        'Y' -> Paper
        'Z' -> Scissor

parseOutcome :: Char -> Outcome
parseOutcome outcome =
    case outcome of
        'X' -> Loss
        'Y' -> Draw
        'Z' -> Win

outcome2 :: Shape -> Outcome -> Shape
outcome2 Rock Win = Paper
outcome2 Paper Win = Scissor
outcome2 Scissor Win = Rock
outcome2 Scissor Loss = Paper
outcome2 Paper Loss = Rock
outcome2 Rock Loss = Scissor
outcome2 opponent _ = opponent

main :: IO ()
main = do
    input <- readFile inputPath
    let parsedInput1 = map (\[opponent,space,you] -> map parseShape [opponent,you]) $ lines input
        points = map roundPoints parsedInput1
        parsedInput2 = map (\[opponent,space,you] -> (parseShape opponent, parseOutcome you)) $ lines input
        points2 = map (\(x,y) -> outcomePoints y + shapePoint (outcome2 x y)) parsedInput2
    print $ sum points
    print $ sum points2