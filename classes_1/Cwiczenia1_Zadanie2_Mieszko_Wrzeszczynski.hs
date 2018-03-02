main :: IO ()
main = return ()

-- Zad 2.a
fun x
 | x > 2 = x * x
 | x > 0 && x <= 2 = x - 1
 | x <= 0 = abs(x)

-- Zad 2.b
gcd_own x 0 = abs x
gcd_own x y = gcd_own b (mod a b)
  where a = abs x
        b = abs y

-- Zad 2.c
lcm_own a b = abs(a * b) `div` gcd_own a b

-- Zad 2.d
trian_checker a b c =  a + b > c && b + c > a && a + c > b

-- Zad 2.e
cone_volume r h = 1/3 * pi * r * r * h

-- Zad 2.f
cone_slant r h = sqrt (r^2 + h^2)

-- Zad 2.g
pow_own a 0 = 1
pow_own a n = a * pow_own a (n-1)

-- pow_ak a n = pow_akPOM a n 1
-- pow_akPOM a n x = if n == 0 then x
--     else pow_akPOM a (n-1) (x*a)

-- Zad 2.h
pow_own_acc a n = pow_acc 1 a n
  where
    pow_acc x a 0 = x
    pow_acc x a b
        | even b = pow_acc x (a*a) (b `div` 2)
        | otherwise = pow_acc (x*a) a (b-1)

-- helper function
fib_gen n = fib_acc n 1 1
  where
    fib_acc 1 x1 x2 = x1
    fib_acc x x1 x2 = fib_acc (x-1) (x1+x2) x1

-- Zad 2.i
fib_10_checker a = a == fib_gen 10

-- Zad 2.j
fib_checker a
  | a < 5  = False
  | a > 100 = False
  | a `elem` [fib_gen x  | x <- [1..100] ] = True
  | otherwise = False
