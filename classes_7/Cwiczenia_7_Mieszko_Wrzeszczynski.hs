-- zad. 7. a
insert' :: Ord a => a -> [a] -> [a]
insert' x [] = [x]
insert' x (y:ys)
  | x < y     = x : y : ys
  | otherwise = y : insert' x ys

insertionSort :: (Ord a) => [a] -> [a]
insertionSort = foldr insert' []

-- zad. 7. b.
bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort [] = []
bubbleSort [x] = [x]
bubbleSort (x:y:xs) = if sorted thisSort then thisSort else bubbleSort thisSort
    where thisSort = (min x y) : bubbleSort ((max x y):xs)

sorted :: (Ord a) => [a] -> Bool
sorted [] = True
sorted [x] = True
sorted (x:y:xs) = if x <= y then sorted (y:xs) else False


-- zad. 10.
data Tree a = Empty | Node a (Tree a) (Tree a)
depth :: Tree a -> Int
depth Empty = 0
depth (Node _ l r) = 1 + max (depth l) (depth r)

min_path :: Tree a -> Int
min_path Empty = 0
min_path (Node _ l r) = 1 + min (min_path l) (min_path r)

max_path :: Tree a -> Int
max_path (Node a l r) = depth (Node a l r)

-- zad. 14.
sumPair (x,y) = abs x + abs y

insertPair x [] = [x]
insertPair x (y:ys)
       | sumPair x < sumPair y = x:y:ys
       | otherwise = y:insertPair x ys

sortPairs [x] = [x]
sortPairs (x:xs) = insertPair x (sortPairs xs)

-- zad. 15.
prime = zip [1..] primes

--indeksujemy od 0
prime' n = primes !! n
primes = filterPrime [2..]
  where filterPrime (p:xs) = p : filterPrime [x | x <- xs, x `mod` p /= 0]
