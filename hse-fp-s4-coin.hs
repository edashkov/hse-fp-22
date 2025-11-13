import System.Random
import System.Environment

{-- We toss n coins until there are some heads. Then remove
 -  as many coins as heads we have got, and continue until
 -  there is no coin. What is the average number of tosses?
 --}

group :: Int -> [Int] -> [[Int]]
group n xs = ys : (group n zs)
             where (ys,zs) = splitAt n xs

trj_length :: Int -> [Int] -> Integer
trj_length 0 _ = 0
trj_length k xs = fromIntegral len + trj_length k' (drop (k * len) xs)
                  where k' = k - sum (head other)
                        len = 1 + length tailss
                        (tailss, other) = span ((0 ==) . sum) . group k $ xs

avg_trj_length :: Int -> Int -> IO Double
avg_trj_length num n = do gens <- sequence . replicate num $ initStdGen
                          let xss = map (randomRs (0, 1) :: StdGen -> [Int]) gens
                          let lens = map (trj_length n) xss
                          return (fromIntegral (sum lens) / fromIntegral (num))

main = do args <- getArgs
          let num = read $ args !! 0
          let n = read $ args !! 1
          avg_trj_length num n >>= print
