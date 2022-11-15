
{-- 
    With no line buffering effect (we have no newlines now), putStrLn writes to an
    invisible buffer before getContents is fully evaluated (hence, the stream is terminated). 
 --}

main = putStrLn "I will change all your symbols to 'A' (press CTRL+D when get tired)" >>
           getContents >>= putStrLn . map (\_ -> 'A')

