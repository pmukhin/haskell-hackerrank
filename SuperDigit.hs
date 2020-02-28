solution :: Integer -> Integer -> Integer
solution x rpl =
    if x < 10 then x else
        solution ((sum $ getDigits' x) * rpl) 1

getDigits' :: Integer -> [Integer]
getDigits' x =
    if x < 10 then [x] else let
        rest = (x `mod` 10) in rest : getDigits' ((x - rest) `div` 10)

main :: IO ()
main = do
    ds  <- (fmap words getLine) :: IO [String] -- IO [String]
    let 
        rpl = readInt (ds !! 1) :: Integer
        num = (ds !! 0) :: String
        in putStrLn . show $ solution (readInt num) rpl
    where
        readInt :: String -> Integer
        readInt = read
