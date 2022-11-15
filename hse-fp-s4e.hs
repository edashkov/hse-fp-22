import Data.Char
import System.Environment
import System.IO

let2int :: Char -> Int
let2int c = ord c - ord ' '

int2let :: Int -> Char
int2let n = chr (n + ord ' ')

shift :: Int -> Char -> Char
shift n c | c == '\n'   = '\n'
          | otherwise   =  int2let $
            ((let2int c) + n) `mod` (ord '~' - ord ' ' + 1)

encode ::  Int -> String -> String
encode n cs = [shift n c | c <- cs]

main = do args <- getArgs 
          if null args then
            hPutStrLn stderr "Error!"
           else do let n = read $ head args
                   s <- getContents
                   let m = n + 1
                   putStr $ encode n s
-- the last do-block's behavior may be not obvious;
-- see hse-fp-l4-q-unsafe* for further discussion.
