-- Eratosthenes' Sieve
sieve :: [Integer] -> [Integer]
--sieve [] = []
sieve (x:xs) = x : sieve [ y | y <- xs, y `mod` x /= 0]

primes = sieve [2..]

n1 = elem 91 primes
n2 = elem 101 primes

-- "QuickSort"
qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = lesser ++ [x] ++ greater
                  where lesser  = qsort [y | y <-xs, y <= x]
                        greater = qsort (filter (> x) xs)

ns1 = qsort [4,5,1,7,12,6]
ns2 = qsort [3,2,3,3,1,5,0,2,1]
ns3 = qsort "What is your favorite PL?"

{-----------------------------------------}




