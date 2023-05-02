import Data.List

{-- Folds and Lazyness --}

-- x1 is polymorphic
x1 = undefined

-- so x2 is Bool
x2 = undefined || True 
x3 = True || undefined
-- (||) is strict in its first argument

x2' = undefined * 0
-- (*) is strict in BOTH arguments
x3' = 0 * undefined

x4 = (\_ -> 2021) undefined

x5 = [True, False] ++ repeat undefined
x6 = take 2 x5
x7 = take 3 x5

x8 = foldr (||) False x5
x8' = or x5

-- foldr f u (x:xs) = f x (foldr f u xs)
-- x8 --> True || foldr (||) False [False, undefined..] --> True || (False || foldr ...)
--             |_______--another way---> True

-- let's MAKE (||) strict in its 2nd arg.:
a |!| b = seq b (a || b) 
-- a common context for seq is as follows:
-- f_strict x = seq x (f x)
y1 = seq undefined ((\_ -> 3) undefined)
-- this is abbreviated to ($!):
y2 = (\_ -> 3) $! undefined
-- f $! x = seq x (f x)

x8a = foldr (|!|) False x5
-- stack overflow, no 'undefined' exception!
-- x8a --> True |!| (foldr (|!|) False [False, undefined..]) -->
--                  True |!| (False |!| (foldr (|!|) False [undefined..]) --> 
--                            True |!| (False |!| (undefined |!| (foldr (|!|) False [undefined..])) --> 
--                                          True |!| (False |!| (undefined |!| (undefined |!| (foldr (|!|) False [undefined..])))                                                   --> ...
-- STACK:  |!| True |!| True  |!| True      |!| True          ...
--                  |!| False |!| False     |!| False
--                            |!| undefined |!| undefined
--                                          |!| undefined
                                      
x8b = foldr (|!|) False $ x7
-- is this any better?
-- x8b -->> True |!| (False |!| (undefined |!| False)) --reduce the 2nd arg-->>  True |!| (False |!| undefined) -->> undefined


-- foldl f u (x:xs) = foldl f (f u x) xs
-- this uses all the memory, yet gives NO stack overflow
x9 = foldl (||) False x5
-- x9 -->  foldl (||) (False || True) [False,undefined..] --> foldl (||) ((False || True) || False) [undefined..] --> ... 
-- nothing goes to foldl function stack (as foldl is tail-recursive, it CANNOT cause stack overflow on itself)
x9a = foldl (|!|) False x5
-- the same here despite (|!|) strictness, since the first arg of foldl is never evaluated

-- What if we make foldl strict in the first argument?
-- foldl' f u (x:xs) = seq u (foldl' f (f u x) xs)
x9b = foldl' (||) False x5
-- ALMOST the same -- as we have
-- foldl' (||) (((False || True) || False) || undefined) [undefined..] -->> foldl' (||) True [undefined..] --> foldl' (||) True [undefined..]
-- a genuine infinite loop on bounded memory, hence NO excessive memory usage

-- let's combine it with strict (|!|)
x9c = foldl' (|!|) False x5
--  foldl (|!|) (((False |!| True) |!| False) |!| undefined) [undefined..] -->> foldl (|!|) undefined [undefined..] --> EXCEPTION

------
-- As for now, we see how strictness influence folds. Let's look at more practical things.
------

-- 100000000 for my desktop
bigList = [1..20000000]

-- let's sum it up...
x10 = foldr (+) 0 bigList
-- (+) is strict in both arguments
-- x10 -->> 0 + (1 + (2 + (foldr (+) 0 [3..100000000] ))) -> ...
--STACK:    0+   0+   0+
--               1+   1+
--                    2+
--
-- stack overflow!


x11 = foldl (+) 0 bigList
-- first, great memory usage (about 10G yet still bounded)
-- THEN stack overflow!
-- Indeed,
-- x11 -->> foldl (+) (((((0 + 1) + 2) + 3) + 4) + ... + 99999999) [100000000] -->> ((((0 + 1) + 2) + 3) + 4) + ... )) + 100000000
--we need lots of space to hold this expression; now it is being evaluated:
--STACK: +100000000 +100000000 +100000000 
--                  +99999999  +99999999
--                             +99999998
-- this can fit the heap yet is too large for the stack!


-- But we can reduce the first argument at every foldl recursion step!
-- foldl' does exactly that:
x12 = foldl' (+) 0 bigList
-- x11 --> foldl' (+) (0 + 1) [2..100000000] --> foldl' (+) 1 [2..100000000] --> foldl' (+) (1 + 2) [3..100000000] --> 
--  foldl' (+) (3) [4..100000000] --> ...
--  No need in a huge memory footprint!

{-- Practical considerations --}

-- Is foldl' always better than foldl?
-- ALMOST!

-- But there may be exceptions!
-- Let's make (||) lazy in its FIRST argument but strict in the second one:
a \| b = flip (||) a b

x13 = foldl' (\|) False [undefined, True]
-- x13 --> foldl' (\|) (False \| undefined) [True] --> foldl' (\|) undefined [True] --> EXCEPTION
x14 = foldl (\|) False [undefined, True]
-- x14 --> foldl (\|) (False \| undefined) [True] --> foldl (\|) ((False \| undefined) \| True) [] --> 
--   ((False \| undefined) \| True) --> True

-- Is foldl' always better than foldr?
-- NO WAY!

-- foldr is the genuine structural recursion on lists and is capable of proceeding infinite lists (unlike foldl and foldl').

-- foldl implicitly reverses a list unlike foldr:
x15 = foldl (flip (:)) [] "qwerty"
x16 = foldr (:) [] "qwerty"


{-- Efficient recursion tricks --}

-- memoize and reuse!

-- Binomial coefficients
-- Exp. time in n -- very inefficient
binom :: Int -> Int -> Integer
binom _ 0 = 1
binom 0 _ = 0
binom n k = binom (n - 1)  k + binom (n - 1) (k - 1)
-- the resulting tree of recursive calls may contain identical values multiple times;
-- it would be beneficial to store them for further usage ('memoization')

-- let's sequentially compute Pascal's Triangle rows!
-- C_n^k = C_{n-1}^{k-1} + C_{n-1}^k = C_{n-1}^{k-1} + ( C_{n-2}^{k-1} + C_{n-2}^k ) = ... = C_{n-1}^{k-1} + ... + C_{k-1}^{k-1}
-- So, (C_n^k)_{n >= k} is the sequence of partial sums for (C_n^{k-1})_{n >= k - 1}
binom' :: Int -> Int -> Integer
binom' _ 0 = 1
binom' 0 _ = 0
binom' n k =  ((iterate (scanl1 (+)) (replicate n 1)) !! k) !! (n - k)


-- Catalan numbers -- let's apply a more general approach
-- a simplistic inefficient implementation is based on the formula C(n) = sum(C(i) * C(n - 1 - i), i = 0..n-1)
catalan :: Int -> Integer
catalan n | n == 0      = 1
          | otherwise   = sum [ catalan i * catalan (n - 1 - i)
                                | i <- [0 .. n-1] ]
-- this is clearly exponential in space and time

-- now, let us memoize all the prevoius values while applying the same naive formula
catalan' :: Int -> Integer
catalan' n = table !! n
             where table = map f [0..n]
                   f m = if m == 0 then 1 else
                            sum [ table !! i * table !! (m - 1 - i)
                                  | i <- [0 .. m-1] ]

-- How does this work?
-- table  -->> [1, table !! 0, table !! 0 * table !! 1 + table !! 1 * table !! 0, ..., table !! 0 * table !! (n-1) + ...]
-- This list is finite and takes space O(n^2) (for normal forms; notice that catalan n = O(4^n)); each thunk is evaluated
-- at its first usage (since (!!) is lazy in its first argument) and then just the value is stored.


