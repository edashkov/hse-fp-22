-- The value 'returned' is discarded according to the Haskell Report 2010 
main :: IO Int

main = do s <- getLine
          let l = length s
--          putStrLn (show l)
          print l  
          return l

