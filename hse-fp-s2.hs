-- 0 
-- Constuct a term of type
-- 1. a -> (a, a)
-- 2. a -> (a -> b) -> b
-- 3. ([Bool],[Int->Int]) -> [Char]

-- 1
-- How many normalizing terms does the type contain?
-- 1. (Bool,Bool,Bool -> Bool) -> Bool -> (Bool,Bool)
-- 2. Char -> (Bool,Char)

-- 2
-- What may the type of e be if
-- 1. e (x,y) = y x
-- 2. e x  = let f = \z -> z 0 in case f x of
--                                True -> x 1
--                                _ -> x 2

--3 Implement via pattern matching
--myOr :: Bool -> Bool -> Bool

-- Let f(x) = 1^x + 2^x + ... + x^x for all natural x. Implement f.

--4 Tinker with the equations 
d1 = head [1,2,3]
d2 = tail [1,2,3]
d3 = [[1,2,3] !! n | n <- [0..3]]
d4 = [1,2,3] !! 3
d5 = last [1,2,3]
d5_5 = init [1,2,3]
d6 = length [1,2,3]
d7 = 5 : [1,2,3]
d8 = 1 : 2 : 3 : []
d9 = [1,2,3] == d8
d10 = [1,2,3] ++ [4,5] ++ [6,7]
d11 = [1,2,3] ++ [4.5]
-- d12 = [1,2,3] ++ "abcde"
d13 = take 2 "abcde"
d14 = drop 2 "abcde"
d15 = reverse "abcde"
d16 = sum [1..9]

--5 Implement the respective functions

--6 Implement replicate and repeat

--7 Bisecting a list
-- halve :: [a] -> ([a],[a])

--8 Generate a list of all integer pairs (x,y) with x^2 - 15 * y^2 = 1

