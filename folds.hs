-- fold with lambda -- 
sum' :: (Num a) => [a] -> a
sum' xs = foldr (\x acc -> x + acc) 0 xs

-- because of currying the above function can be re-written as 
sum'' xs = foldr (+) 0 xs

-- constructing `elem` using folds 
elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc n -> if n == y then True else acc) False ys
