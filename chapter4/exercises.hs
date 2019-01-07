compare’ :: Ord a => [a] -> [a] -> Ordering
compare’ [] [] = EQ
compare’ (x:xs) (y:ys) | length (x:xs) < length (y:ys) = LT
| length (x:xs) > length (y:ys) = GT
| otherwise = compare (x:xs) (y:ys)

compare' :: Ord a => [a] -> [a] -> Ordering
compare' [] [] = EQ
reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs : x 


difference' :: [a] -> [a] -> [a]
difference' s [] = s
difference' (x:xs) s | elem x s = difference' xs s
                     | otherwise = x : difference' xs s

genUnion :: [[a]] -> [a]
genUnion [] = []
genUnion (x:xs) = ... append x to genUnion xs




