-- zad. 1.
powerlist [] = [[]]
powerlist (x:xs) = [x:px | px <- powerlist(xs)] ++ powerlist(xs)

-- zad. 2.
iloczyn_z [] _ = []
iloczyn_z (x:xs) ys
   | x elem ys = x:iloczyn_z xs ys
   | otherwise = iloczyn_z xs ys

-- zad. 3.
unique [] = []
unique (x:xs) = x : unique(filter (x /=) xs)

suma_z x y = unique (x ++ y)

-- zad. 4.
{-

-- a
foldr (/) 2 [6,12,24,8]
6 / foldr (/) 2 [12,24,8]
6 / (12 / foldr (/) 2 [24,8])
6 / (12 / (24 / foldr (/) 2 [8])
6 / (12 / (24 / (8 / foldr (/) 2 []))
6 / (12 / (24 / (8 / 2))))
6 / (12 / (24 / 4))
6 / (12 / 6)
6 / 2
3

-- b
foldr (&&) True [1>2,3>2,5==5]
1>2 && (foldr (&&) True [3>2,5==5])
1>2 && (3>2 && (foldr (&&) True [5==5]))
1>2 && (3>2 && (5==5 && (foldr (&&) True [])))
1>2 && (3>2 && (5==5 && True))
1>2 && (3>2 && True)
1>2 && True

-- c
foldr max 18 [3,6,12,4,55,11]
3 max (foldr max 18 [6,12,4,55,11])
3 max (6 max (foldr max 18 [12,4,55,11]))
3 max (6 max (12 max (foldr max 18 [4,55,11])))
3 max (6 max (12 max (4 max (foldr max 18 [55,11]))))
3 max (6 max (12 max (4 max (55 max (foldr max 18 [11])))))
3 max (6 max (12 max (4 max (55 max (11 max (foldr max 18 []))))))
3 max (6 max (12 max (4 max (55 max (11 max 18)))))

-- d
foldr max 81 [3,6,12,4,55,11]
3 max (foldr max 81 [6,12,4,55,11])
3 max (6 max (foldr max 81 [12,4,55,11]))
3 max (6 max (12 max (foldr max 81 [4,55,11])))
3 max (6 max (12 max (4 max (foldr max 81 [55,11]))))
3 max (6 max (12 max (4 max (55 max (foldr max 81 [11])))))
3 max (6 max (12 max (4 max (55 max (11 max (foldr max 81 []))))))
3 max (6 max (12 max (4 max (55 max (11 max 81)))))

-- e
foldr (\x y -> (x+y)/2) 54 [24,4,10,6]
(24 + foldr (\x y -> (x+y)/2) 54 [4,10,6])/2
(24 + (4 + foldr (\x y -> (x+y)/2) 54 [10,6])/2)/2
(24 + (4 + (10 + foldr (\x y -> (x+y)/2) 54 [6])/2)/2)/2
(24 + (4 + (10 + (6 + foldr (\x y -> (x+y)/2) 54 [])/2)/2)/2)/2
(24 + (4 + (10 + (6 + 54)/2)/2)/2)/2

-- f
foldl (\x y -> (x+y)/2) 54 [2,4,10,6]
foldl (\x y -> (x+y)/2) (54+2)/2 [4,10,6]
foldl (\x y -> (x+y)/2) ((54+2)/2 + 4)/2 [10,6]
foldl (\x y -> (x+y)/2) (((54+2)/2 + 4)/2 + 10)/2 [6]
foldl (\x y -> (x+y)/2) ((((54+2)/2 + 4)/2 + 10)/2 + 6)/2 []

-- g
foldl (/) 64 [4,2,4]
foldl (/) (64 / 4) [2,4]
foldl (/) ((64 / 4) / 2) [4]
foldl (/) (((64 / 4) / 2)/4) []

-- h
foldl (\x y -> 2*x + y) 8 [1,2,3]
foldl (\x y -> 2*x + y) (2*8 + 1) [2,3]
foldl (\x y -> 2*x + y) (2*(16 + 1) + 2) [3]
foldl (\x y -> 2*x + y) (2*(2*(16 + 1) + 2) + 3) []
(2*(2*(16 + 1) + 2) + 3)
(2*(2*17 + 2) + 3)
(2*36 + 3)
(72 + 3)
75
-}

-- zad. 5.
nalezy x e = (foldl (\c d -> c + (if d == e then 1 else 0) ) 0 x) > 0

-- zad. 6.
map'' f xs = foldl (\acc value -> acc ++ [(f value)]) [] xs

-- zad. 7.
last_own xs = foldr1 (\acc value -> value) xs
head_own xs = foldr1 (\value acc -> value) xs
max_own xs = foldr1 (\value acc -> if value > acc then value else acc ) xs

-- zad. 8.
zip_own _ [] = []
zip_own [] _ = []
zip_own (g1 : o1) (g2 : o2) = (g1 , g2) : zip_own o1 o2

unzip_own [] = ([], [])
unzip_own ((a,b):xs) = (a:(fst (unzip_own xs)), b:(snd (unzip_own xs)))

-- zad. 9.
even_own xs = filter (\x -> even x ) xs
{-
even_own [1,2,3,4,5,6] -> [2,4,6]
-}

power_two xs = map (\x -> x^2 ) xs
{-
power_two [-1,2,3,4,5] -> [1,4,9,16,25]
-}

sum_own xs = foldr (\x acc -> x + acc) 0 xs
{-
sum_own [1,2,3,4,5] -> 15
-}

add_x_each c xs = map (\x -> x + c) xs
{-
add_x_each 10 [1,2,3] -> [11,12,13]
-}

count_x e x = foldl (\c d -> c + (if d == e then 1 else 0) ) 0 x
{-
count_x 1 [1,2,3,4,10,1,1,1,1] -> 5
-}

sqrt_map xs = map (\x -> sqrt x) xs
{-
sqrt_map [1,2,3,4,5,6,36,144] -> [1.0,1.4142135623730951,1.7320508075688772,2.0,2.23606797749979,2.449489742783178,6.0,12.0]
-}
