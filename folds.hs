-- fold with lambda -- 
sum' :: (Num a) => [a] -> a
sum' xs = foldr (\x acc -> x + acc) 0 xs

-- because of currying the above function can be re-written as 
sum'' xs = foldr (+) 0 xs

-- constructing `elem` using folds 
elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc n -> if n == y then True else acc) False ys

-- constructing map using foldr and foldl
map' :: (a -> b) -> [a] -> [b]
map' p xs = foldr (\x acc -> p x: acc) [] xs

map'' :: (a -> b) -> [a] -> [b]
map'' p xs = foldl (\acc x -> acc ++ [p x]) [] xs -- ++ is an expensive operation so the implementation of map using foldr is better

-- constructing filter sieve 
filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = foldr (\x acc -> if p x then x : acc else acc) [] xs

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p xs = foldl (\acc x -> if p x then acc ++ [x] else acc) [] xs

-- maximum using foldr1 
maximum' :: (Ord a) => [a] -> a
maximum' xs = foldr1 (\x acc -> if x > acc then x else acc) xs
 
-- reverse using foldl
reverse' :: [a] -> [a]
reverse' xs = foldl (\acc x -> x:acc) [] xs 

reverse'' xs = foldl (flip (:)) [] xs

-- product using foldr1
product' :: (Num a) => [a] -> a
product' xs = foldr1 (*) xs

-- implementing head with folds
head' :: [a] -> a
head' xs = foldl1 (\acc _ -> acc) xs -- or foldr1 (\x _ -> x) xs

-- implementing last with folds
last' :: [a] -> a
last' xs = foldr1 (\_ acc -> acc) xs -- or foldl1 (\_ x -> x) xs

-- alternate list sum
alternatelistsum :: (Num a)=> [a]-> a
alternatelistsum xs = foldl (\acc x -> x - acc) 0 xs -- alternatelistsum [1,2,3,4,5,6] => -1+2-3+4-5+6 => 3

alternatelistsum2 :: (Num a)=> [a]-> a
alternatelistsum2 xs = foldr (\x acc -> x - acc) 0 xs -- alternatelistsum2 [1,2,3,4,5,6] => 1-2+3-4+5-6 => -3


-- SCAN  (outputs values of accumulator in fold expressions) -------------------
scanr (+) 0 [1,2,3,4,5] -- [15,14,12,9,5,0] (accumulate from right)
scanl (+) 0 [1,2,3,4,5] -- [0,1,3,6,10,15] (accumulate from lhs)
scanl1 (\acc x -> if x > acc then x else acc) [3,4,5,3,7,9,2,1] -- [3,4,5,5,7,9,9,9] (max in a list)
scanl (flip (:)) [] [1,2,3] -- [[],[1],[2,1],[3,2,1]]

-- number of terms for which (sqrt x1 + sqrt x2 + sqrt x3 ... < thresh)
sqrtSums :: (Num a, Floating a, Ord a, Enum a) => a -> Int
sqrtSums thresh = length (takeWhile (< thresh) (scanl1 (\acc x -> acc + sqrt x) [1..]))






