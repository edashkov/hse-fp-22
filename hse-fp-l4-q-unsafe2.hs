import System.IO.Unsafe

-- this does not work as (>>=) is strict in its first argument for the IO monad:
-- the infinite _sequence_ of actions must be performed before passing the resulting
-- string to takeWhile
getLinePut1 :: IO ()
getLinePut1 = sequence (repeat getChar) >>= putStrLn . takeWhile (/= '\n')

-- it is possible to make it lazy however:
lazySequence :: [IO a] -> IO [a]
lazySequence [] =  return []
lazySequence (x:xs) = do r <- x
                         rs <- unsafeInterleaveIO $ lazySequence xs
                         return (r:rs)
-- (unsafeInterleaveIO a) is the same action as a itself but it is performed
-- on demand; the order of execution of a sequence of such actions is thus
-- hard to predict since it is governed by the 'demand' and is interleaved
-- by executing the caller's code. For this reason, lazy IO is considered
-- 'unsafe'. When reading (and not writing), such a behavior is safe enough to
-- use lazy IO in the standard implementations of getContents, readFile, etc.
getLinePut2 = lazySequence (repeat  getChar) >>= putStrLn . takeWhile (/= '\n')


{-- More observations on strictness of (>>=)  --}

-- return x is like \state -> (state, x) -- see the source; this 'action' is
-- indeed performed before the right-hand side evaluation 
g3 = ((return ['A'..'z']) :: IO String) >>= putStrLn . takeWhile (/= 'P')
-- perform multiple actions (finitely many):
g3' = (sequence . map return $ ['A'..'z']) >>= putStrLn . takeWhile (/= 'P')

-- still finitely many actions; notice that newlines are "ignored"
-- until the actions in the left-hand side are complete
g4 = sequence (replicate 5 getChar ++ [undefined]) >>= putStrLn . takeWhile (/= '\n')

-- now, the actions in the right-hand side are performed lazily: we actually read a character
-- not earlier than we are ready to compare it with '\n' and put it to stdout.
-- That's why the characters are visibly doubled and newlines are taken into account immediately. 
g4' = (sequence . map unsafeInterleaveIO $ (replicate 5 getChar ++ [undefined])) >>= putStrLn . takeWhile (/= '\n')

-- similar to g4':
g4'' = lazySequence (replicate 5 getChar ++ [undefined]) >>= putStrLn . takeWhile (/= '\n')

-- this behaves similarly to g4: we have ONE (sequenced) action which is performed 'later' than in g4,
-- but its steps are performed strictly one by one (without passing control to the right-hand side in between);
-- ineed, we have strict (>>=) INSIDE the implementation of sequence
g4''' = (unsafeInterleaveIO . sequence $ (replicate 5 getChar ++ [undefined])) >>= putStrLn . takeWhile (/= '\n')


-- here we have ONE action (for return) performed, which results in an infinite (lazy) list
g5 = (return [1..] :: IO [Int]) >>= print . takeWhile (<= 42)

-- on the other hand, here we have to perform INFINITELY MANY actions (packed to one by sequence)
-- prior to passing the resulting list any further.
g6 = (sequence . map return $ [1..]) >>= print . takeWhile (<= 42)

-- similar to g5
g6' = (lazySequence . map return $ [1..]) >>= print . takeWhile (<= 42)

