-- #1
get' :: [a] -> Int -> a
get' [] _ = error "Index out of range."
get' (x:xs) 0 = x 
get' (x:xs) n | n >= 0 = get' xs (n - 1)
              | otherwise = error "Negative index!"

-- #2
head' :: [a] -> a
head' [] = error "Empty list!"
head' (x:xs) = x 

-- #3
last' :: [a] -> a
last' [] = error "Empty list!"
last' [x] = x
last' (_:xs) = last' xs

-- #4
tail' :: [a] -> [a]
tail' [] = error "Empty list!"
tail' (x:xs) = xs

-- #5
init' :: [a] -> [a]
init' [] = error "Empty list!"
init' [x] = []
init' (x:xs) = x: init' xs

-- #6
reverse' :: [a] -> [a]
reverse' xs = helper xs [] where 
                                         helper [] a = a
                                         helper (x:xs) a = helper xs (x:a)

-- #7
lenght' :: [a] -> Int
lenght' [] = 0
lenght' xs = helper xs 0 where
                                helper [] n = n
                                helper (_:xs) n = helper xs (n+1)

-- #8
append' :: [a] -> a -> [a]
append' [] y = [y]
append' (x:xs) y = x: append' xs y 

-- #9
concat' :: [a] -> [a] -> [a]
concat' [] ys = ys
concat' (x:xs) ys = x: concat' xs ys

-- #10
drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' 1 (x:xs) = xs
drop' n (x:xs) | n <= 0 = xs
               | otherwise = drop' (n-1) xs

-- #11
take' :: Int -> [a] -> [a]
take' _ [] = []
take' 1 (x:xs) = [x]
take' n (x:xs) | n > 0 = x: take' (n-1) xs
               | otherwise = []

-- #12
splitAt' :: Int -> [a] -> ([a],[a])
splitAt' n xs = (take' n xs, drop' n xs)

-- #13
null' :: [a] -> Bool 
null' [] = True
null' _ = False 

-- #14
elem' :: (Eq a) => [a] -> a -> Bool
elem' [] _ = False 
elem' (x:xs) y | x == y = True
               | otherwise = elem' xs y

-- #15
filter' :: (a -> Bool) -> [a] ->  [a]
filter' _ [] = []
filter' test (x:xs) | test x = x: filter' test xs 
                    | otherwise = filter' test xs

-- #16
map' :: (a -> b) -> [a] -> [b]
map' _ [] = [] 
map' f (x:xs) = f x: map' f xs

-- #17
zip' :: [a] -> [a] -> [(a,a)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y): zip' xs ys
