-- helpers
gcd_own x 0 = abs x
gcd_own x y = gcd_own b (mod a b)
  where a = abs x
        b = abs y

lcm_own a b = abs(a * b) `div` gcd_own a b

-- zad. 2.
main = do putStrLn "Podaj dwie liczby"
          line <- getLine
          line2 <- getLine
          let x = read line
              y = read line2
          putStrLn (show(x + y))
          putStrLn (show(x * y))
          putStrLn (show(x - y))

-- zad. 3.
zad3 = do putStrLn "Podaj dwie liczby"
          line <- getLine
          line2 <- getLine
          let x = read line
              y = read line2
          putStrLn (show(gcd_own x y))
          putStrLn (show(lcm_own x y))

-- zad. 4.
zad4 = do putStrLn "Podaj dwie liczby"
          name <- getLine
          surname <- getLine
          putStrLn ([head name] ++ [head surname])

-- zad. 5.
zad5 :: IO ()
zad5 = do
         putStr "Guess a number between 0 and 100: "
         let n = 66
         loop n 0

loop :: Int -> Int -> IO ()
loop n i = do
           input <- getLine
           let g = read input :: Int

           if i == 10
              then do
                  putStrLn "Too late!"
           else if g < n
           then do
                  putStrLn "Too low; guess again: "
                  loop n (i+1)
           else if g > n
           then do
                  putStrLn "Too high; guess again: "
                  loop n (i+1)
           else do
                   putStrLn "Correct!"
