
module GS 

where 

divides :: Integer -> Integer -> Bool
divides d n = rem n d == 0

ld :: Integer -> Integer 
ld n = ldf 2 n 

ldf :: Integer -> Integer -> Integer 
ldf k n | divides k n = k 
        | k^2 >  n    = n 
        | otherwise   = ldf (k+1) n

prime0 :: Integer -> Bool
prime0 n | n < 1     = error "not a positive integer"
         | n == 1    = False 
         | otherwise = ld n == n

mnmInt :: [Int] -> Int
mnmInt [] = error "empty list" 
mnmInt [x] = x
mnmInt (x:xs) = min x (mnmInt xs)

min' :: Int -> Int -> Int 
min' x y | x <= y    = x
         | otherwise = y 

average :: [Int] -> Rational 
average [] = error "empty list" 
average xs = toRational (sum xs) / toRational (length xs)

sum' :: [Int] -> Int
sum' [] = 0 
sum' (x:xs) = x + sum' xs

length' :: [a] -> Int
length' [] = 0 
length' (x:xs) = 1 + length' xs

prefix :: String -> String -> Bool
prefix [] ys = True
prefix (x:xs) [] = False
prefix (x:xs) (y:ys) = (x==y) && prefix xs ys 

factors :: Integer -> [Integer]
factors n | n < 1     = error "argument not positive"
          | n == 1    = []
          | otherwise = p : factors (div n p) where p = ld n 

a = 3
b = 4 
f :: Integer -> Integer -> Integer
f x y = x^2 + y^2

g :: Integer -> Integer 
g 0     = 1
g x = 2 * g (x-1)

h1 :: Integer -> Integer 
h1 0 = 0
h1 x = 2 * (h1 x) 

h2 :: Integer -> Integer 
h2 0 = 0
h2 x = h2 (x+1) 

removeFst :: Int -> [Int] -> [Int]
removeFst m [] = []
removeFst m (x:xs) 
  | x == m = xs
  | otherwise = x : removeFst m xs 

srtInts :: [Int] -> [Int]
srtInts [] = []
strInts xs = m : (srtInts (removeFst m xs)) where m = (mnmInt xs)

srtInts' :: [Int] -> [Int]
srtInts' [] = []
srtInts' xs = 
    let m = (mnmInt xs )
    in m : (srtInts' (removeFst m xs))

countChar :: Char -> String -> Int
countChar ch [] = 0
countChar ch (c:cs)
  | ch == c = 1 + (countChar ch cs)
  | otherwise = 0 + (countChar ch cs)

multString :: Integer -> Char -> [Char]
multString n c
  | n == 0 = []
  | otherwise = c : (multString (n - 1) c)

blowupHelper :: Integer -> [Char] -> [Char]
blowupHelper n [] = []
blowupHelper n (x:xs) = (multString n x) ++ (blowupHelper (n + 1) xs)
              
blowup :: String -> String
blowup str = blowupHelper 1 str



