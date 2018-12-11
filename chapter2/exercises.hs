-- HELPERS

infix 1 ==>

(==>) :: Bool -> Bool -> Bool
x ==> y = (not x) || y

infix 1 <=>

(<=>) :: Bool -> Bool -> Bool
x <=> y = x == y 

infixr 2 <+>

(<+>) :: Bool -> Bool -> Bool
x <+> y = x /= y 

logEquiv2 :: (Bool -> Bool -> Bool) -> 
    (Bool -> Bool -> Bool) -> Bool
logEquiv2 bf1 bf2 = 
    and [(bf1 p q) <=> (bf2 p q)  |  p <- [True,False], 
                                     q <- [True,False]]

logEquiv3 :: (Bool -> Bool -> Bool -> Bool) ->
                 (Bool -> Bool -> Bool -> Bool) -> Bool                                   
logEquiv3 bf1 bf2 = 
  and [(bf1 p q r) <=> (bf2 p q r) |  p <- [True,False], 
                                      q <- [True,False], 
                                      r <- [True,False]] 

-- exercise 2.15
notValid1 :: (Bool -> Bool) -> Bool
notValid1 bf = and [not (bf p ) | p <- [True,False]]

--Exercise 2.16
-- 1) The equation does not have a solution
-- 2) A largest natural number does exits
-- 3) The number 13 is not prime
-- 4) he number n is not prime

-- Exercise 2.17
-- x < y < z = (x < y) ^ (y < z)
-- ~(x < y) ^ (y < z) = by deMorgan, (x >= y) V ( y >= z)

-- Exercise 2.18
-- Part 1
-- A <--> B = by definition
-- A -> B ^ B -> A = by contrapositive
-- ~B -> ~A ^ ~A -> ~B = by flipping rule
-- ~A->~B ^ ~B -> ~A = by definition
-- ~A <--> ~B
-- Part 2, Show ~A <--> B TRIPLE A <--> ~B
-- ~A <--> B = by definition
-- ~A -> B ^ B -> ~A = by contrapositive
-- ~B -> A ^ A -> ~B = by flipping
-- A -> ~B  ^ ~B -> A  = by definition
-- A <--> ~B

--Exercise 2.19
--SKIP

--Exercise 2.20
-- 1 - False
-- 2 - False
-- 3 - True
formula41 p q r = p ==> (q ==> r)
formula42 p q r = q ==> (p ==> r)
-- 4 - True
formula51 p q r = p ==> (q ==> r)
formula52 p q r = (p ==> q) ==> r
-- 5 - False
-- SKIP

--Exercise 2.21 
--SKIP

--Exercise 2.22 
--SKIP

--Exercise 2.23
-- See Notes

-- Exercise 2.26
-- See Notes

-- Exercise 2.27
-- See Notes

-- Example 2.30
[ x | x <- list, property x ]

-- [ 2^n + 1 | n <- [0..], prime (2^n + 1) ]
-- [ 2^n + 1 | n <- [0..], not (prime (2^n + 1)) ]



