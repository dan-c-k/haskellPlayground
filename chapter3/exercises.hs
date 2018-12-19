prime :: Integer -> Bool
prime n 
| n < 1 = error "not a positive integer"
| n == 1 = False
| otherwise = ldp n == n where

ldp = ldpf primes
ldpf (p:ps) m 
| rem m p == 0 = p
| p^2 > m = m
| otherwise = ldpf ps m

-- primes = 2 : filter prime [3..] 


examples = [take n primes | n <- [0..],
                            not (prime (product (take n primes) + 1))]