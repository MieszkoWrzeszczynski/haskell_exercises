module MojZbior (podzbior,iloczyn,suma,roznica) where
iloczyn [] _ = []
iloczyn (x:xs) ys
   | x elem ys = x:iloczyn xs ys
   | otherwise = iloczyn xs ys

unique [] = []
unique (x:xs) = x : unique(filter (x /=) xs)

suma x y = unique (x ++ y)
-- roznica xs \ y
roznica xs y = filter (\x -> not (x `elem` y)) xs
-- czy 2 jest podzbiorem 1
podzbior xs y = length(y) == length(filter (\x -> x `elem` y) xs)
