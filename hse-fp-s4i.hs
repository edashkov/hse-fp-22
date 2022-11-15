import System.IO
import System.Random

main = do gen <- getStdGen
          putStrLn $ take 16 (randomRs ('A','z') gen)
          gen' <- getStdGen
          putStrLn $ take 16 (randomRs ('A','z') gen')

