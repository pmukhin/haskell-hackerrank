import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe

solution :: String -> String
solution x = run x Map.empty where
    run :: String -> Map Char Bool -> String
    run [] _ = []
    run (x:xs) lp =
        let find = Map.lookup x lp in
            if isJust find then run xs lp else
                x : run xs (Map.insert x True lp)

main :: IO ()
main = fmap solution getLine >>= putStrLn