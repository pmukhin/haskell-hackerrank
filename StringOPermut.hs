import Data.List

solution :: String -> String
solution [x] = [x]
solution [] = []
solution (x:y:xs) =
    y:x:solution xs

main :: IO ()
main = do
    cnt    <- fmap readInt getLine
    result <- (sequence $ replicate cnt getLine)
    putStrLn $ intercalate "\n" (map solution result)
    where
        readInt :: String -> Int
        readInt = read