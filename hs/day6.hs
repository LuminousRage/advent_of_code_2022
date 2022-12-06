
import Data.List

inputPath :: String
inputPath = "./input/input_6.txt"

startPacket :: Int -> Int -> String -> Maybe Int
startPacket pos num msg
    | marker == num = Just $ pos + num
    | length msg < num = Nothing
    | otherwise = startPacket (pos+1) num $ tail msg
    where   marker = length $ nub $ take num msg

main = do
    input <- readFile inputPath
    print $ startPacket 0 4 input
    print $ startPacket 0 14 input