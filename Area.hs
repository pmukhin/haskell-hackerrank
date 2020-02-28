import qualified Data.Map.Strict (Map)
import Data.Map.Strict as Map
import Data.Maybe

isInt x = x == fromInteger (round x)

solution :: Double -> Double -> Int
solution p d = rc 1
    where 
        rc :: Double -> Int
        rc curr = let leftp = curr ** p in
            if leftp == d then 1
            else
                if leftp > d then 0
                else
                    (if isInt $ (d - leftp) ** (1/p) 
                        then (1 + (solution p curr)) 
                        else 0) + rc (curr+1)

solution' :: Double -> Double -> [(Double, Double)]
solution' p d = rc 1
    where 
        rc :: Double -> [(Double, Double)]
        rc curr = let leftp = curr ** p in
            if leftp == d then [(curr, 0)]
            else
                if leftp > d then []
                else
                    (if isInt $ (d - leftp) ** (1/p) then [(curr, (d - leftp) ** (1/p))] else []) ++ rc (curr+1)

main :: IO ()
main = do
    x <- fmap readInt getLine
    y <- fmap readInt getLine
    putStrLn $ show $ solution' x y
    where 
        readInt :: String -> Double
        readInt = read