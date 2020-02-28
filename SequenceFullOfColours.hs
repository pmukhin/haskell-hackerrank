import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List
import Data.Maybe

balls :: Char -> Map Char Int -> Int
balls x lp = fromMaybe 0 (Map.lookup x lp)

solution :: String -> Bool
solution x = run x Map.empty where
    run :: String -> Map Char Int -> Bool
    run [] mp = (balls 'R' mp) == (balls 'G' mp) && 
        (balls 'Y' mp) == (balls 'B' mp)
    run (x:xs) mp = 
        let nmp = Map.insertWith (+) x 1 mp in 
            if      abs ((balls 'R' nmp) - (balls 'G' nmp)) > 1 then False
            else if abs ((balls 'Y' nmp) - (balls 'B' nmp)) > 1 then False
            else    run xs nmp

main :: IO ()
main = do
    cnt <- fmap readInt getLine
    wds <- sequence $ replicate cnt getLine
    putStrLn $ intercalate "\n" $ map (show . solution) wds
    where
        readInt :: String -> Int
        readInt = read