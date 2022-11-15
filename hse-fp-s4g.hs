import Data.Char
import System.Environment
import System.IO

main = getArgs >>=
    (\args -> withFile (head args) ReadMode 
        (\handle -> hGetContents handle >>= hPutStr stdout . unlines . map reverse . lines))

