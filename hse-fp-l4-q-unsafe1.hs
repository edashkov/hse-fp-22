
{-- We know that (>>=) is strict in its first argument in the IO monad.
    But getContents (and some other file-reading procedures) makes an exclusion.
 --}

main = putStrLn "I will change your lines' symbols to 'A' (press CTRL+D when get tired)" >>
           getContents >>= putStrLn . map (\c -> if c == '\n' then c else 'A')

{--
 The IO in getContents is LAZY (the so-called 'unsafe IO' is used in this function's
 implementation). It means that each symbol is read only when it is explicitly 'ordered' by
 the right-hand side of >>=. By default, putStrLn applies 'Line Buffering', that is,
 it waits for a newline before it begins printing symbols. As we have kept the newlines
 from stdin, at every '\n', putStrLn starts handling the previous symbols, hence these
 symbols are read by getContents. 
--}
  
