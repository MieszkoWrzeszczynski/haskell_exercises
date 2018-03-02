-- Zad1.a
add a xs = a:xs

-- Zad1.b
add_sec a (x:xs) = x:a:(xs)

-- Zad1.c
add_last a xs = xs ++ [a]

-- Zad2.a
get_second (x:xs) = head xs

-- Zad2.b
get_three (x:xs) = head (tail xs)

-- Zad2.c
get_penultimate (a:[b]) = a
get_penultimate (x:xs) = get_penultimate xs

-- Zad3.a
reverse_own (x:[]) = [x]
reverse_own (x:xs) = reverse_own xs ++ [x]

-- Zad4
swap (x:xs) = last xs : tail xs ++ [x]

--Zad5.a
even_plus xs = length (filter even (filter (>0) xs))

--Zad5.b
mod_3 x = x `mod` 3 == 0
mod_e n = length (filter mod_3 [1..n])

--Zad5.c
mod_sum n = sum (filter mod_3 [1..n])

-- Zad.6
even_len xs = length xs `mod` 2 == 0

-- Zad7
power_list xs = map  (\x -> x^2) xs
power_list_without_map xs = [ x^2 | x <- xs]

-- Zad8
count a xs  = length (filter (\x -> x == a) xs)

-- Zad9
duplicate x n = [x | _ <- [1..n]]

-- Zad10
pal xs = reverse xs == xs

-- Zad11
remove_first _ [] = []
remove_first a (x:xs) | a == x = xs
                      | otherwise = x : remove_first a xs
-- Zad12
remove_n n xs = remove_temp n xs 0
remove_temp 0 (x:xs) _ = xs
remove_temp n (x:xs) i | n == i = xs
                       | otherwise = x : remove_temp n xs (i+1)

-- Zad13
same x y = length (filter (\z -> z `elem` x) y) == length y

-- Zad14
swap_tuple z = map (\(x,y) -> (y,x)) z
