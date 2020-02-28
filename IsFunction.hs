import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe 

readInt :: String -> Int
readInt = read

solution :: [[Int]] -> Bool
solution [] = True
solution xs = 
    run Map.empty xs where 
        run :: Map Int Int -> [[Int]] -> Bool
        run cache [] = True
        run cache (x:xs) =
            let 
                key = x !! 0
                val = x !! 1
                cch = Map.lookup key cache in
                    if isJust cch then (fromJust cch) == val && run cache xs
                    else run (Map.insert key val cache) xs

solutionM :: IO ()
solutionM = do
    pairs <- fmap readInt getLine
    arr   <- sequence $ replicate pairs readarr 
    putStrLn $ if (solution arr) then "YES" else "NO"
        where
            readarr :: IO [Int]
            readarr = fmap (map readInt . words) getLine

main :: IO ()
main = do
    tc <- fmap readInt getLine
    sequence_ $ replicate tc solutionM