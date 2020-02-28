data Point = Point Double Double

compute :: [Point] -> Double
compute (x:xs) = cmpt x xs where
    cmpt _ []  = 0
    cmpt _ [x] = 0
    cmpt x (y:y':ys) =
        let 
            (a, b, c) = ((distance x y), (distance x y'), (distance y y')) 
            s         = (a+b+c) / 2 in
                sqrt (s*(s-a)*(s-b)*(s-c)) + cmpt x (y':ys)
    
distance :: Point -> Point -> Double
distance (Point x y) (Point x' y') =
    sqrt (((x - x') ** 2) + ((y - y') ** 2))

fromList :: [Double] -> Point
fromList xs =
    if (length xs) /= 2 then error "bullshit" else Point (xs !! 0) (xs !! 1)

main :: IO ()
main = do
    pts <- fmap readInt getLine
    arr <- sequence $ replicate pts (fmap (fromList . (map readFloat . words)) getLine)
    putStrLn $ show $ compute arr
    where
        readInt :: String -> Int
        readInt = read
        readFloat :: String -> Double
        readFloat = read