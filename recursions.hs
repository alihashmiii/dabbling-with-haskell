prod [] = 1
prod (x:xs) = x * prod xs

factorial n = prod [1..n]

len [] = 0
len (_:xs) = 1 + len xs

reversals [] = []
reversals (x:xs) = reversals xs ++ [x]

zipfun :: [x] -> [x] -> [(x,x)]
zipfun [] _ = []
zipfun _ [] = []
zipfun (x:xs) (y:ys) = (x,y):zipfun xs ys

dropfun :: Int -> [a] -> [a]
dropfun _ [] = []
dropfun 0 xs = xs
dropfun n (_:xs) = dropfun (n-1) xs

(+++) :: [a] -> [a] -> [a]
[] +++ ys = ys
(x:xs) +++ ys =  x:(xs +++ ys)

qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where
                  smaller = [n | n <- xs, n <= x]
                  larger = [n | n <- xs, n > x]

andlist :: [Bool] -> Bool
andlist [] = True
andlist (False:_) = False
andlist (_:xs) = andlist xs

concatrecur :: [[a]] -> [a]
concatrecur (y:[]) = y
concatrecur (x:xs) = [n | n <-x] ++ concatrecur xs

repelem :: Int -> a -> [a]
repelem 0 _ = []
repelem n r = r : repelem (n-1) r

-- select nth element
(x:_) !!! 0 = x
(_:xs) !!! n = xs !!! (n-1)

elementq :: Eq a => a -> [a] -> Bool
elementq _ [] = False
elementq el (x:xs) | el /= x = elementq el xs
                   | otherwise = True

mapf1 f xs = [f x | x <- xs]

mapf_ f [] = []
mapf_ f (x:xs) = f x : mapf_ f xs

filter1 f xs = [x | x <- xs, f x]

filter_ f [] = []
filter_ f (x:xs) | f x  = x:(filter_ f xs)
                 | otherwise = filter_ f xs

sumlist [] = 0
sumlist (x:xs) = x + sumlist xs

productlist [] = 1
productlist (x:xs) = x * productlist xs

andlist2 [] = True
andlist2 (x:xs) | x = andlist2 xs
                | otherwise = False

andlist_ [] = True
andlist_ (x:xs) = x && andlist_ xs

takewhile _ [] = []
takewhile p (x:xs) | p x = x:takewhile p xs
                   | otherwise = []

dropwhile _ [] = []
dropwhile p list@(x:xs) | p x = dropwhile p xs
                   | otherwise = list
                   
                   
                   
-- from LYAH ---------------------------------------------------------

maximum' [] = error "max of []"
maximum' [x] = x
maximum' (x:y:[])
                 | x > y = x
                 | otherwise = y
maximum' (x:y:xs)
                 | x > y = maximum' (x:xs)
                 | otherwise = maximum' (y:xs)


maximum'' :: (Ord a) => [a] -> a
maximum'' [] = error "max of emptylist"
maximum'' [x] = x
maximum'' (x:xs)
                 | x > maxTail = x
                 | otherwise = maxTail
                 where maxTail = maximum'' xs

 -- best implementation for maximum
maximum''' :: Ord a => [a] -> a
maximum''' [] = error "max of emptylist"
maximum''' [x] = x
maximum''' (x:xs) = max x (maximum''' xs)

-- replicate
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' 0 _ = []
replicate' n expr = expr : (replicate' (n-1) expr)

replicate'' :: (Num i, Ord i) => i -> a -> [a]
replicate'' n expr
                   | n == 0 = []
                   | otherwise = expr : replicate'' (n-1) expr


-- take with patterns --
take' :: (Num i ,Ord i) => i -> [a] -> [a]
take' n _ | n == 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

-- reverse --
reverse' :: Ord a => [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse xs ++ [x]

-- repeat -- infinite list - lazy evaluation
repeat' :: Ord a => a -> [a]
repeat' x = x:repeat' x

-- zip --
zip' :: Ord a => [a] -> [a] -> [(a,a)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y): (zip' xs ys)

-- elem --
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' n (x:xs)
              | n == x = True
              | otherwise = n `elem'` xs


-- quicksort --
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort smaller ++ [x] ++ quicksort larger
                    where smaller = [n | n <- xs, n < x]
                          larger  = [m | m <- xs, m >= x]

quicksort' :: Ord a => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) = let smaller = [n | n <- xs, n < x]
                        larger  = [m | m <- xs, m >= x]
                    in quicksort' smaller ++ [x] ++ quicksort' larger

