import Data.Char
import System.Environment
import System.IO

main = do args <- getArgs
          let pin = head args
              pout = head $ tail args
          hin <- openFile pin ReadMode
          input <- hGetContents hin
          let output = unlines . map reverse . lines $ input
          writeFile pout output
          hClose hin

-- withFile, hGetLine,...          
