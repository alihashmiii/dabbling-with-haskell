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
