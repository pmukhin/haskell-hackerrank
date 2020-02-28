-- Enter your code here. Read input from STDIN. Print output to STDOUT
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibr 0 1 n 
    where 
        fibr l r 1 = r
        fibr l r n = 
            fibr r (l + r) (n - 1)


readInt :: String -> Int
readInt = read

bgm :: Int
bgm = 7 + (round (10 ** 8))

fibonacciM :: IO ()
fibonacciM = do
    n <- fmap readInt getLine
    putStrLn $ show $ (fibonacci n) `mod` bgm

main :: IO ()
main = do
    cs <- fmap readInt getLine
    sequence_ $ replicate cs fibonacciM