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
             | n >= 2 = fibonacci2(n-2) + fibonacci2(n-1) 

f :: [Int] -> [Int]
f (_:x:xs) = x : f xs
f _ = []

rev [] = []
rev (h:t) = rev t ++ [h]

revNum :: Int -> String
revNum n | n < 10 = show n 
         | otherwise = show (n `mod` 10) ++ revNum (n `div` 10)

sumOddElems arr = sum (filter odd arr)

len :: [a] -> Int
len lst = length lst

leng :: [a] -> Int
leng [] = 0
leng (x:xs) = 1 + leng xs

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
    where g x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x : xs)
    | p x       = x : filter p xs
    | otherwise = filter p xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) = 
    let smaller = quicksort (filter (<=x) xs)
        bigger = quicksort  (filter (> x) xs)
    in  smaller ++ [x] ++ bigger
 