import Data.List

solution :: String -> [String]
solution x = run (length x) x where
    run 0 _ = []
    run n xs = let s = (tail xs)++[head xs] in s : run (n-1) s

main :: IO ()
main = do 
    cnt <- fmap readInt getLine
    sequence_ $ replicate cnt solutionM
    where
        readInt :: String -> Int
        readInt = read
        solutionM :: IO ()
        solutionM = 
            (fmap solution getLine) >>= putStrLn . intercalate " "
