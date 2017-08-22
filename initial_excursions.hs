quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x:xs) = quicksort ys ++ [x] ++ quicksort zs
             where
             ys = [y | y <- xs,  y <= x]
             zs = [z | z <- xs, z > x]

fibonacci :: Integer -> Integer
fibonacci n
    | n == 0 = 0
    | n == 1 = 1
    | n > 1 = fibonacci (n-1) + fibonacci (n-2)

square :: Int -> Int
square x = x * x

add :: Num a => (a, a) -> a
add (x,y) = x + y

plus :: Int -> Int -> Int
plus x y = x + y

addM :: (Int,Int) -> Int
addM = \(x,y) -> x + y

zeroto :: Int -> [Int]
zeroto n = [0..n]

-- add x = \y -> x+y

-- add = \x -> \y -> x+y

mult :: Int -> Int -> Int -> Int
mult x y z = x*y*z

second :: [a] -> a
second xs = head (tail xs)

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

pair :: t -> t1 -> (t,t1)
pair x y = (x,y)

double :: Num a => a -> a
double x = x*2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (t -> t) -> t -> t
twice f x = f (f x)

-- chapter 4

--signumber :: Int -> Int
--signumber n = if n < 0 then -1 else
--                 if n == 0 then 0 else 1

absolute n | n >= 0  = n
      | otherwise = -n

signumber n | n < 0 = -1
            | n == 0 = 0
            | otherwise  = 1

nott :: Bool -> Bool
-- nott x | x == True = False
--        | otherwise = True
nott True = False
nott False = True

(&&&) :: Bool -> Bool -> Bool
True &&& True = True
_ &&& _ = False

(&&&&) :: Bool -> Bool -> Bool
True &&&& b = b
False &&&& _ = False

headexp :: [a] -> a
headexp (x:_) = x

tailexp :: [a] -> [a]
tailexp [] = []
tailexp (_:xs) = xs

constant :: a -> b -> a
constant x _ = x
-- more naturally defined as
--constant :: a -> (b -> a)
--constant x = \_ -> x

odds :: Int -> [Int]
odds n = map (\x -> x*2 + 1) [0..n-1]

(|||) :: Bool -> Bool -> Bool
True ||| _ = True
_ ||| True = True
False ||| False = False

(||||) :: Bool -> Bool -> Bool
False |||| b = b
True |||| _ = True

andlogical :: Bool -> Bool -> Bool
andlogical x y | x == y = x
               | otherwise = False

andlogical2 :: Bool -> Bool -> Bool
andlogical2 x y = if x == y then x else
                  False

concatenate :: [[a]] -> [a]
concatenate xss = [x | xs <- xss, x <- xs]

pickeven :: Integral a => a -> [a]
pickeven a = [ x | x <- [1..a], even x ]

factors :: Integral a => a -> [a]
factors a = [ x | x <- [1..a] , a `mod` x == 0]

prime :: Int -> Bool
prime x = factors x == [1,x]

primes :: Int -> [Int]
primes num = [x | x <- [2..num] , prime x]

adjacentpairs :: [a] -> [(a, a)]
adjacentpairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x,y) <- adjacentpairs xs]

pos x xs = [i | (xt,i) <- zip xs [0..n], x == xt]
           where n = length xs - 1

evenp (_,0) = False
evenp (_,b) = b `mod` 2 == 0
evenpos xs = filter evenp (zip xs [0..])

pytriples n = [(x,y,z) | x <- [1..n], y <- [1..x], z <- [1..y], x^2 == (y^2 + z^2) ]

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

-- higher order func (composition and foldr) --
twicef :: (a -> a) -> (a -> a)
twicef f = f.f

lengthls :: [a] -> Int
lengthls = sum.map(\_ -> 1)

sump xs = foldr (+) 0 xs
prodp xs = foldr (*) 1 xs
oR xs = foldr (||) False xs
truep xs = foldr (&&) True xs

lengthlist xs = foldr (\ _ x -> x + 1) 0 xs

-- reverse implementation using fold --
reversal_r xs = foldr (\ x vec -> vec ++ [x]) [] xs -- 1:(2:(3:[])) -> (([] ++ [3]) ++ [2]) ++ [1] -> [3,2,1]
reversal_l xs = foldl (\ vec x -> [x] ++ vec) [] xs -- 1:(2:(3:[])) -> [3] ++ ([2] ++ ([1] ++ [])) -> [3,2,1]

-- append using fold --
(##) xs ys = foldr (:) ys xs -- shorterversion (##) = foldr (:)

-- any p xs AND all p xs are examples of higher order functions, similarly takeWhile and dropWhile are also powerful constructs

-- map and filter as foldr --
mapfold p xs = foldr (\x vec -> p x : vec) [] xs
filterfold p xs  = foldr (\x vec -> if p x then x:vec else vec)  [] xs

-- parsers
item :: [t] -> [(t, [t])]
item = \inp -> case inp of
                 []     -> []
                 (x:xs) -> [(x,xs)]

p +-+ q = (\inp -> case p inp of
                   []        -> parse q inp
                   [(v,out)] -> [(v,out)])

-- check on REPL: (item +-+ return 'd') "$ab" -> [('$',"ab")]
-- check on REPL: (failure +-+ return 'd') "$ab" -> [('d',"$ab")]

parse p inp = p inp

failure = \ inp -> []
returnpar v = \inp -> [(v,inp)]

--- below is a nice implementation of a list of partial functions formed from lazy evaluation
h = map (*) [1..] -- i.e. [(0*) (1*) (2*) ...]
-- test (h !! 10) 5 = 55

-- $ or function application with low precedence than space. this can be used to avoid adding paranthesis
sum $ filter (>10) $ map (*2) [2..10] -- same as sum (filter (> 10) (map (*2) [2..10]))
