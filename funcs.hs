reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

fibonacci :: Integer -> Integer
fibonacci n | n == 0    = 0
            | n == 1    = 1
            | n >= 2    = fibonacci2(n-2) + fibonacci2(n-1)             
            | otherwise = error "arg must be >=0"

fibonacci2 :: Integer -> Integer
fibonacci2 n | n == 0   = 0
             | n == 1   = 1
             | n < 0    = fibonacci2(n+2) - fibonacci2(n+1)
             | n >= 2   = fibonacci2(n-2) + fibonacci2(n-1)             