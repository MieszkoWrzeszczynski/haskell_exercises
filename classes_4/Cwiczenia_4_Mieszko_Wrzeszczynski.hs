-- zad. 1.
-- a
data Marka = Volvo | Audi | Fiat | Bugati | Seat
  deriving(Show)

data Kraj = Polska | Anglia | Litwa | USA
data Moto = Marka String [String]

nameOfCountry :: Kraj -> Marka
nameOfCountry d = case d of
                    Polska -> Audi
                    Anglia -> Fiat
                    Litwa -> Seat
                    USA -> Bugati

-- b
avg_speed :: Marka -> Float
avg_speed d = case d of
                    Audi -> 250.4
                    Volvo -> 190.4
                    Fiat -> 150
                    Seat -> 200.4
                    Bugati -> 320.5

-- zad. 2.
preorder :: Tree a -> [a]
preorder Empty = []
preorder (Node a l r) = [a] ++ preorder l ++ preorder r

inorder :: Tree a -> [a]
inorder Empty = []
inorder (Node a l r) = inorder l ++ [a] ++ inorder r

postorder :: Tree a -> [a]
postorder Empty = []
postorder (Node a l r) = postorder l ++ postorder r ++ [a]

data Tree a = Empty | Node a (Tree a) (Tree a)
  deriving(Show)

-- a
t :: Tree Int
t = Node 1 (Node 2 (Node 4 Empty Empty)
        (Node 5 Empty (Node 8 Empty Empty)))
    (Node 3 (Node 6 Empty (Node 9 Empty Empty))
        (Node 7 Empty Empty))


-- -- preorder t
-- t3 = Node 1 (Node 2 (Node 4 Empty Empty)
--         (Node 5 Empty Empty))
--     (Node 3 (Node 6 Empty (Node 9 Empty Empty))
--         (Node 7 Empty Empty))

-- preorder t
-- [1,2,4,5,8,3,6,9,7]

-- postorder  t
-- [4,8,5,2,9,6,7,3,1]

-- inorder   t
-- [4,2,5,8,1,6,9,3,7]

-- b
t1 :: Tree Char
t1 = Node 'a' (Node 'b'  Empty
        (Node 'd'  (Node 'f' Empty Empty) Empty) )
     (Node 'c' (Node 'e' Empty (Node 'g' Empty Empty))
         Empty)

-- inorder  t1
-- "bfdaegc"
-- preorder   t1
-- "abdfceg"
-- postorder t1
-- "fdbgeca"

-- zad. 3.
treeMemberIn tr mem = mem `elem` (inorder tr)
treeMemberPre tr mem = mem `elem` (preorder tr)
treeMemberPost tr mem = mem `elem` (postorder tr)


-- zad. 4.
-- to execute subTree eg. subTree t t4
t4 :: Tree Int
t4 = Node 3 (Node 6 Empty Empty)
     (Node 7 Empty Empty)

-- helper fun
subset :: Eq a => [a] -> [a] -> Bool
subset xs y = length(y) == length(filter (\x -> x `elem` y) xs)

subTree :: Eq a => Tree a -> Tree a -> Bool
subTree a  b = subset (inorder a) (inorder b) && subset (postorder a) (postorder b)

-- zad. 5.
poziomo :: Tree a -> [a]
poziomo tree = tbf [tree]
    where
        tbf [] = []
        tbf xs = map nodeValue xs ++ tbf (concat (map leftAndRightNodes xs))
        nodeValue (Node a _ _) = a
        leftAndRightNodes (Node _ Empty Empty) = []
        leftAndRightNodes (Node _ Empty b)     = [b]
        leftAndRightNodes (Node _ a Empty)     = [a]
        leftAndRightNodes (Node _ a b)         = [a,b]
