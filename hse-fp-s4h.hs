import System.Random

{-- Randomized qsort --}

rnddel :: [a] -> IO (a,[a])
rnddel xs = do n <- randomRIO (0, length xs - 1)
               return $ (xs !! n, take n xs ++ drop (n+1) xs)

rqsort :: (Ord a) => [a] -> IO [a]
rqsort [] = return []
rqsort xs = do (x,ys) <- rnddel xs
               lesser  <- rqsort [y | y <-ys, y <= x]
               greater <- rqsort [y | y <-ys, y > x]
               return $ lesser ++ [x] ++ greater
                     

