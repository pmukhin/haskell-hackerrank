import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.List

data MatchType = Frag | Complete deriving Show

build :: [String] -> Map String MatchType
build [] = Map.empty
build (w:ws) = 
    Map.union (fromWord w 1) (build ws) where
        fromWord :: String -> Int -> Map String MatchType
        fromWord w n = 
            if n == (length w) then Map.fromList [(w, Complete)]
            else Map.union (Map.fromList [(take n w, Frag)]) (fromWord w (n+1))

solution :: [String] -> String -> [String]
solution allpwd givpwd = 
    let 
        lp = build allpwd 
        r = run lp givpwd "" in 
            if head r == "WRONG" then ["WRONG","PASSWORD"] else reverse r
        where
            run :: Map String MatchType -> String -> String -> [String]
            run lp [] _ = []
            run lp (x:xs) acc = 
                let val = Map.lookup (acc++[x]) lp in
                    if isNothing val then ["WRONG","PASSWORD"]
                    else 
                        let r = fromJust val in case r of 
                            Frag -> run lp xs (acc++[x])
                            Complete -> run lp xs [] ++ [acc++[x]]


main :: IO ()
main = do
    c <- fmap readInt getLine
    sequence_ $ replicate c mainM where
        readInt :: String -> Int
        readInt = read
        mainM :: IO ()
        mainM = do
            _   <- getLine
            allpwd <- fmap words getLine
            givpwd <- getLine
            putStrLn $ intercalate " " (solution allpwd givpwd)
                