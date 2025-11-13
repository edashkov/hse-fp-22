main = print $ let xs = [1..20000000] in sum $ (xs ++ xs) ++ xs
