solution :: [[Float]] -> Float
solution a = 
  (perim (a ++ [head a])) / 2 where
    perim [x] = 0
    perim (x:y:xs) = 
      let 
        x1 = x !! 0
        x2 = x !! 1
        y1 = y !! 0
        y2 = y !! 1 in 
          (x1 * y2 - y1 * x2) + perim (y:xs)

main :: IO ()
main = do
  x   <- fmap getInt getLine
  arr <- sequence $ replicate x getLine
  putStrLn $ show $ solution (fmap (map getFloat . words) arr)
  where
    getInt :: String -> Int
    getInt = read
    getFloat :: String -> Float
    getFloat = read