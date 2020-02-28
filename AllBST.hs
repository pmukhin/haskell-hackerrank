import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Ord, Show, Eq)

insrt :: (Ord a) => Tree a -> a -> Tree a
insrt Leaf a = Node a Leaf Leaf

insrt (Node a l r) new =
    if new < a then (Node a (insrt l new) r) else (Node a l (insrt r new))

isBst :: (Ord a) => Tree a -> Bool
isBst Leaf = True
isBst (Node a l r) = case (l, r) of 
    (Leaf, Leaf) -> True
    ((Node vl l' r'), Leaf) -> 
        a > vl && isBst l' && isBst r'
    (Leaf, (Node vr l' r')) ->
        a < vr && isBst l' && isBst r'
    ((Node vl l' r'), (Node vr l'' r'')) ->
      a > vl && a < vr && isBst l' && isBst r' && isBst l'' && isBst r''

buildTree :: [Int] -> Tree Int
buildTree xs = 
    foldl insrt Leaf xs

alltrees :: [[Int]] -> IO ()
alltrees xs = putStrLn $ intercalate "\n" (fmap show (filter isBst $ map buildTree xs))

solution :: [Int] -> Int
solution xs = 
    let perm = permutations xs in 
        length $ filter (==True) $ map isBst $ Set.toList $ Set.fromList (map buildTree perm)

main :: IO ()
main = do 
    c   <- fmap getInt getLine
    arr <- sequence $ replicate c (fmap getInt getLine)
    sequence_ $ map (\x -> (putStrLn $ show $ (solution [1..x]))) arr
    where
        getInt :: String -> Int
        getInt = read