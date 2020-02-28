import Data.List

data Pos = Pos Int Int Int

splitOn d [] = []
splitOn d wrd = 
    run d wrd [] where
        run :: Char -> String -> String -> [String]
        run d [] acc = [acc]
        run d (x:xs) acc =
            if x == d then acc : run d xs [] else run d xs (acc++[x])
            
solution :: [String] -> [String] -> [String]
solution fld wrd = undefined

main :: IO ()
main = do
    fld <- sequence $ replicate 10 (getLine)
    wrd <- fmap (splitOn ';') getLine
    putStrLn $ show $ solution fld wrd