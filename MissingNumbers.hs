import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List

sumt :: (Int, Int) -> (Int, Int) -> (Int, Int)
sumt (x, y) (x', y') = (x + x', y + y')

findDiff :: [(Int, (Int, Int))] -> [Int]
findDiff [] = []
findDiff (x:xs) = let 
    d   = fst x
    n   = (fst . snd) x
    n'  = (snd . snd) x
    in (if n == n' then [] else [d]) ++ findDiff xs

solution :: [Int] -> [Int] -> [Int]
solution x y = let lp = run x y Map.empty in 
    findDiff $ Map.toList lp
    where
        run :: [Int] -> [Int] -> Map Int (Int, Int) -> Map Int (Int, Int)
        run [] [] lp = lp
        run x [] lp =
            run (tail x) [] (Map.insertWith sumt (head x) (1, 0) lp)
        run [] y lp =
            run [] (tail y) (Map.insertWith sumt (head y) (0, 1) lp)
        run x y lp = let 
            wx = (Map.insertWith sumt (head x) (1, 0) lp)
            wy = (Map.insertWith sumt (head y) (0, 1) wx) in
                run (tail x) (tail y) wy

main :: IO ()
main = do
    x   <- fmap readInt getLine
    xl  <- fmap ((map readInt) . words) getLine
    x'  <- fmap readInt getLine
    xl' <- fmap ((map readInt) . words) getLine
    putStrLn $ intercalate " " (fmap show $ solution xl xl')
    where
        readInt :: String -> Int
        readInt = read