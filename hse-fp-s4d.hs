import System.Environment

main = do
    args <- getArgs
    progName <- getProgName
    putStrLn "The number of arguments is:"
    print $ length args
    putStrLn "The arguments are:"
    mapM putStrLn args
    putStrLn "The program name is:"
    putStrLn progName
