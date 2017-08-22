-- product of list members
prod [] = 1
prod (x:xs) = x * prod xs

-- factorial
factorial n = prod [1..n]

factorial' 0  = 1
factorial' n = n * factorial (n-1)

-- length of list
len [] = 0
len (_:xs) = 1 + len xs

-- zip lists
zipfun :: [x] -> [x] -> [(x,x)]
zipfun [] _ = []
zipfun _ [] = []
zipfun (x:xs) (y:ys) = (x,y):zipfun xs ys

-- drop elements
dropfun :: Int -> [a] -> [a]
dropfun _ [] = []
dropfun 0 xs = xs
dropfun n (_:xs) = dropfun (n-1) xs

-- append
(+++) :: [a] -> [a] -> [a]
[] +++ ys = ys
(x:xs) +++ ys =  x:(xs +++ ys)

-- concatenate  
concatrecur :: [[a]] -> [a]
concatrecur (y:[]) = y
concatrecur (x:xs) = [n | n <-x] ++ concatrecur xs

-- repeat element 
repelem :: Int -> a -> [a]
repelem 0 _ = []
repelem n r = r : repelem (n-1) r

-- select nth element from list
(x:_) !!! 0 = x
(_:xs) !!! n = xs !!! (n-1)

-- element? of list
elementq :: Eq a => a -> [a] -> Bool
elementq _ [] = False
elementq el (x:xs) | el /= x = elementq el xs
                   | otherwise = True

mapf1 f xs = [f x | x <- xs] -- list comprehension defining map

-- recursive implementation of map
mapf_ f [] = []
mapf_ f (x:xs) = f x : mapf_ f xs

filter1 f xs = [x | x <- xs, f x] -- list comprehension defining filter 

-- recursive implementation of filter
filter_ :: (a -> Bool) -> [a] -> [a]
filter_ _ [] = []
filter_ f (x:xs) | f x  = x:(filter_ f xs)
                 | otherwise = filter_ f xs

-- summation of list elements
sumlist [] = 0
sumlist (x:xs) = x + sumlist xs

-- productlist
productlist [] = 1
productlist (x:xs) = x * productlist xs

-- andlist, True if all True
andlist_ [] = True
andlist_ (x:xs) = x && andlist_ xs

andlist2 [] = True
andlist2 (x:xs) | x = andlist2 xs
                | otherwise = False

andlist :: [Bool] -> Bool
andlist [] = True
andlist (False:_) = False
andlist (_:xs) = andlist xs

-- takewhile
takewhile _ [] = []
takewhile p (x:xs) | p x = x:takewhile p xs
                   | otherwise = []

-- dropwhile 
dropwhile _ [] = []
dropwhile p list@(x:xs) | p x = dropwhile p xs
                   | otherwise = list
                   
                   
                   
-- from LYAH ----------------------------------------------------------------------------------------------------------
-- my implementation for maximum
maximum' [] = error "max of []"
maximum' [x] = x
maximum' (x:y:[])
                 | x > y = x
                 | otherwise = y
maximum' (x:y:xs)
                 | x > y = maximum' (x:xs)
                 | otherwise = maximum' (y:xs)

-- another implementation of maximum from book
maximum'' :: (Ord a) => [a] -> a
maximum'' [] = error "max of emptylist"
maximum'' [x] = x
maximum'' (x:xs)
                 | x > maxTail = x
                 | otherwise = maxTail
                 where maxTail = maximum'' xs

 -- best implementation for maximum from book
maximum''' :: Ord a => [a] -> a
maximum''' [] = error "max of emptylist"
maximum''' [x] = x
maximum''' (x:xs) = max x (maximum''' xs)

-- replicate an expression n times
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

-- zip function
zip' :: Ord a => [a] -> [a] -> [(a,a)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y): (zip' xs ys)

-- element of a list
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

-- quicksort (similar as above but with let ) --
quicksort' :: Ord a => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) = let smaller = [n | n <- xs, n < x]
                        larger  = [m | m <- xs, m >= x]
                    in quicksort' smaller ++ [x] ++ quicksort' larger

-- quicksort (using filter func) --
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = let smaller = filter (<=x) xs
                       bigger  = filter (>x) xs
                   in quicksort smaller ++ [x] ++ quicksort bigger



