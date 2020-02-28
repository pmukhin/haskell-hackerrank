import qualified Data.Map.Strict (Map)
import Data.Map.Strict as Map
import Data.Maybe
import Data.List

-- Enter your code here. Read input from STDIN. Print output to STDOUT
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibr 0 1 n 
    where 
        fibr l r 1 = r
        fibr l r n = 
            fibr (r `mod` bgm) ((l + r) `mod` bgm) (n - 1)

fibcached :: Int -> Map Int Int -> (Map Int Int, Int)
fibcached n cache = 
    let cached = Map.lookup n cache in 
        if isJust cached then (cache, fromJust cached)
        else let r = fibonacci n in (Map.insert n r cache, r)

readInt :: String -> Int
readInt = read

bgm :: Int
bgm = 7 + (round (10**8))

dowhile :: [Int] -> Map Int Int -> [Int] 
dowhile [] mp = []
dowhile (x:xs) mp = 
    let (nmp, f) = fibcached x mp in
        f : (dowhile xs nmp)

main :: IO ()
main = do
    cs  <- fmap readInt getLine
    arr <- sequence $ replicate cs (fmap readInt getLine)
    putStrLn $ intercalate "\n" (fmap show (dowhile arr (Map.empty)))
