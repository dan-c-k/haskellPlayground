:multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

zipWhip' :: (a -> b ->c) -> [a] -> [b] -> [c]
zipWhip' _ [] _ = []
zipWhip' _ _ [] = []
zipWhip' f (x:xs) (y:ys) = f x y : zipWhip' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
   where g x y = f y x

-- $ is useful for applying to list of functions or for right chaining

   
