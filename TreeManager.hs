import Debug.Trace

data Tree a = Leaf 
            | Node a [Tree a] (Tree a) Int
            deriving Show

data Cmd = Change Integer 
         | Print
         | InsertChild Integer
         | InsertLeft  Integer
         | InsertRight Integer
         | VisitLeft
         | VisitParent
         | VisitRight
         | VisitChild  Int
         | Delete
         deriving Show

strToCmd :: String -> Cmd
strToCmd x = run $ words x where 
    run ["print"]     = Print
    run ("change":xs) = Change (readInteger $ head xs)
    run ("visit":xs)  = doVisit  xs
    run ("insert":xs) = doInsert xs
    run ["delete"]    = Delete

doVisit :: [String] -> Cmd
doVisit [x] = case x of 
    "left"   -> VisitLeft
    "right"  -> VisitRight
    "parent" -> VisitParent
doVisit (x:xs) = 
    VisitChild (readInt $ head xs)

doInsert :: [String] -> Cmd
doInsert (x:xs) = case x of 
    "left"   -> InsertLeft p
    "right"  -> InsertRight p
    "child"  -> InsertChild p
    where p = (readInteger $ head xs)

readInteger :: String -> Integer
readInteger = read

readInt :: String -> Int
readInt = read

solution :: [Cmd] -> Tree Integer -> IO ()
solution [] _ = return ()

solution (x:xs) Leaf = case x of 
    (Change v) -> solution xs (Node v [] Leaf (-1))

solution (x:xs) cur@(Node v children par@(Node parval parchildren parpar parpos) pos) = 
    case x of 
        Delete           -> error "error not implemented"
        Print            -> (putStrLn $ show v) *> solution xs cur
        (Change nv)      -> solution xs (Node nv children (Node parval parchildren parpar parpos) pos)
        VisitLeft        -> solution xs (parchildren !! (pos-1))
        VisitRight       -> solution xs (parchildren !! (pos+1))
        VisitParent      -> solution xs par
        (VisitChild   n) -> solution xs (children !! (n-1))
        (InsertLeft  nv) -> solution xs (insertLeft  cur nv)
        (InsertRight nv) -> solution xs (insertRight cur nv)
        (InsertChild nv) -> solution xs (insertChild nv cur)

change :: Integer -> Tree Integer -> Tree Integer
change nv Leaf = (Node nv [] Leaf (-1))
change nv (Node v children par pos) = 
    let newn = 

updatePos   

insertLeft :: Tree Integer -> Integer -> Tree Integer
insertLeft (Node a children (Node parval parchildren parpar parpos) pos) nv = 
    let 
        leftparchildren  = if pos == 0 then [] else take pos parchildren
        rightparchildren = if pos == 0 then parchildren else drop pos parchildren
        newnode = (Node nv [] parpar pos)
        newparchildren = leftparchildren ++ [newnode] ++ (fmap uppos rightparchildren)
        newpar = (Node parval newparchildren parpar parpos)
        in (Node a children newpar pos)

insertRight :: Tree Integer -> Integer -> Tree Integer
insertRight (Node a children (Node parval parchildren parpar parpos) pos) nv = 
    let 
        leftparchildren  = if pos == 0 then [] else take (pos+1) parchildren
        rightparchildren = if pos == 0 then parchildren else drop (pos+1) parchildren
        newnode = (Node nv [] parpar pos)
        newparchildren = leftparchildren ++ [newnode] ++ (fmap uppos rightparchildren)
        newpar = (Node parval newparchildren parpar parpos)
        in (Node a children newpar pos)

insertChild :: Integer -> Tree Integer -> Tree Integer
insertChild val cur@(Node v children par pos) = 
    let nnode = Node val [] cur (length children) in
        (Node v (nnode:(fmap uppos children)) par pos)

uppos :: Tree a -> Tree a
uppos (Node v xs par pos) = (Node v xs par (pos+1))

main :: IO ()
main = do
    cnt  <- fmap readInt getLine
    cmds <- sequence $ replicate cnt getLine
    solution (fmap strToCmd cmds) Leaf