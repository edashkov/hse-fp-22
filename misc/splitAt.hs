splitAt' _ [] = ([],[])
splitAt' n xs | n <= 0 = ([], xs)
splitAt' n (x:xs) = (x:l, h)
                    where (l,h) = splitAt' (n-1) xs


splitAt'' n = foldr (\(k,x) p -> if k < n then (x:fst p, snd p) else ([],x:snd p)) ([],[]) . zip [0..]

halve xs = (l, h)
            where (_,l,h) = foldr (\(k,x) (ln,l,h) -> if k < ln then (ln,x:l,h) else (ln+1,[],x:h)) (0,[],[]) $ zip [0..] xs


