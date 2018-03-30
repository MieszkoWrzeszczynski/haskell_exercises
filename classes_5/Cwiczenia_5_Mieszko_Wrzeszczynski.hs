import MojZbior

-- zad. 2.
data Tree a = Empty | Node a (Tree a) (Tree a)

inorder :: Tree a -> [a]
inorder Empty = []
inorder (Node a l r) = inorder l ++ [a] ++ inorder r

checker :: Tree Int -> Bool
checker (Node a l r) = length(filter(\x -> x < a ) (inorder l)) == length(inorder l)
                       && length(filter(\x -> x > a ) (inorder r)) == length(inorder r)

-- zad. 3.
depth :: Tree a -> Int
depth Empty = 0
depth (Node _ l r) = 1 + max (depth l) (depth r)

min_path :: Tree a -> Int
min_path Empty = 0
min_path (Node _ l r) = 1 + min (min_path l) (min_path r)


max_path :: Tree a -> Int
max_path (Node a l r) = depth (Node a l r)


-- zad. 4.




class Adres a where
 adres :: a -> Bool

data Email = EmailAddress Int
  deriving(Show)

instance Adres Int where
  adres 0 = True
  adres _ = False

unique [] = []
unique (x:xs) = x : unique(filter (x /=) xs)

-- zad. 5.
count_occur xs = foldl (\acc d -> unique(acc ++ [(d,length(filter(\x -> x == d) xs))]) ) [] xs
