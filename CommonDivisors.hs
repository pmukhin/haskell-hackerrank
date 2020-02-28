solution :: Int -> Int -> Int
solution x y =
    let maxdiv = if x `mod` y == 0 then y else x `mod` y in 
        (run maxdiv) 
        where
            run :: Int -> Int
            run 0 = 1
            run 1 = 1
            run n =
                (if x `mod` n == 0 && y `mod` n == 0 then 1 else 0) + run (n-1) 

main :: IO ()
main = do
    x   <- fmap readInt getLine
    arr <- fmap (fmap words) (sequence $ replicate x getLine)
    sequence_ $ map (putStrLn . lnch) arr
    where 
        lnch :: [String] -> String 
        lnch xs = show $ (uncurry solution) tpl where
            x = readInt $ xs !! 0
            y = readInt $ xs !! 1
            tpl = if x > y then (x, y) else (y, x)
        readInt :: String -> Int
        readInt = read