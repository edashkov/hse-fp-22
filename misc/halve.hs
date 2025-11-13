import System.Environment (getArgs)

splitAt' _ [] = ([],[])                                                                                                                           
splitAt' n xs | n <= 0 = ([],xs)                                                                                                                  
splitAt' n (x:xs) = (x:p, s)                                                                                                                      
                    where (p,s) = splitAt' (n-1) xs                                                                                               
                                                                                                                                                  
splitAt'' n  = foldr (\(i,x) pair -> if i <= n then (x:fst pair,snd pair) else ([],x:snd pair)) ([],[]) . zip ([1..] :: [Int])                    
                                                                                                                                                  
halve0 xs = splitAt (length xs `div` 2) xs                                                                                                        
halve1 xs = splitAt' (length xs `div` 2) xs                                                                                                       
halve2 xs = splitAt'' (length xs `div` 2) xs
halve3 xs = (p,s) where
            helper :: [a] -> ([a],[a],Int)
            f (i,x) = \(p,s,len) -> if i <= len then (x:p,s,len) else ([],x:s,1+len)
            helper = foldr f ([],[],0) . zip ([1..] :: [Int])
            (p,s,_) = helper xs

main = do args <- getArgs
          let alg_num = read (args !! 0) :: Int
          let n = read (args !! 1) :: Int
          let bigList = [1..n]
          let algos = [halve0, halve1, halve2, halve3]
          print $ (\(l,r) -> sum l + sum r) $ (!!) algos alg_num bigList
