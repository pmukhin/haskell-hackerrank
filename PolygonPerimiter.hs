compute :: [[Float]] -> Float
compute [] = 0
compute (x:xs) = 
    run ((x:xs) ++ [x]) where
        run :: [[Float]] -> Float
        run [x] = 0
        run (x:y:xs) =
            distance (x !! 0, x !! 1) (y !! 0, y !! 1) + run (y:xs)

distance :: (Float, Float) -> (Float, Float) -> Float
distance (x, y) (x', y') =
    sqrt (((x - x') ** 2) + ((y - y') ** 2))

main :: IO ()
main = do
    pts <- fmap readInt getLine
    arr <- sequence $ replicate pts (fmap (map readFloat . words) getLine)
    putStrLn $ show $ compute arr
    where
        readInt :: String -> Int
        readInt = read
        readFloat :: String -> Float
        readFloat = read
