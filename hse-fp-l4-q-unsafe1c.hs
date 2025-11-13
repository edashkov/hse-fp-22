import System.IO

{--
 With stdout buffering being disabled, one can clearly
 see that getContents reads characters lazily: whenever
 they are demanded by the right-hand side of >>=.
 As stdin buffering is enabled, putStrLn actually gets
 symbols on newline occurences in stdin.
 --}

main = hSetBuffering stdout NoBuffering >> hSetBuffering stdin NoBuffering >>
        putStrLn "I will change all your symbols to 'A' (press CTRL+D when get tired)" >>
           getContents >>= putStrLn . map (\_ -> 'A') 

