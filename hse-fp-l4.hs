import System.Random
-- try
-- $ cabal install --lib random
-- in case of problems

{-- The IO --}

{-- So far, every term (with its 'arguments' included)
-- has been 'pure', that is, its evaluation depends
-- solely on the term itself.
-- For example, 2 + 2 ALWAYS evaluates to 4 while
-- map (^2) [2..] evaluates to [4,9,16,25,...].

-- But often the evaluation of a program depends on
-- or changes something beyond this program scope.
-- For example, a program may 
-- (1) read from or write to a memory location; while
-- the location may be an argument of the program, the
-- value read is not directly specified but depends on
-- the memory state; likewise, the value written is not
-- what the program 'returns' (evaluates to).
-- (2) read from or write to some external 'IO stream';
-- (3) be influenced by hardware interrupts, signals,
-- user interaction, etc.  

-- In a word, the program may interact with the world 
-- out of its scope. Hence, it may produce DIFFERENT
-- computations depending on the state of that 'world'
-- with EFFECTS on the latter.

-- How can one DESCRIBE such phenomena in our declarative,
-- mathematics-like, equation-style programming laguage?

-- There exists a special formal machinery ('monads') for
-- this task. Now, let us focus on IO and consider it from
-- a practical angle.
--} 

-- Basically, we want to introduce some terms that represent
-- IO subroutines (such terms are known as (IO) 'actions').
-- At the lowest level, we shall have a set of 'black-box' terms
-- for elementary operations which are  provided by the system
-- (e.g., 'to read a character from stdin'). We want to be able to
-- combine such elementary operations into imperative style subroutines,
-- still being expressible as actions.

-- Let IO a be THE type for an IO action that 'returns' a
-- value of type a. If it returns nothing, we shall conventionally
-- use IO ().

-- In reality, IO a is a primitive implemented at the system level.
-- However, we can imagine (and formally describe to some extent) it
-- to be like

-- type IO a = World -> (World, a) 

-- Indeed, an IO operation generally depends on the state of the World
-- and may change it, while returning some value of type a.

-- NOTICE: in fact, we are NOT going to supply any World argument to
-- a term of the type (IO a) since this "argument" is out of our scope.
-- However one can imagine such an argument provided automatically
-- during the run time, so that 'executing'
--      x <- act :: IO a
-- boils down to computing something like
--      (new_state_of_the_world, x) = act current_state_of_the_world
-- with, of course, 'changing the world' effect
--      current_state_of_the_world <- new_state_of_the_world 


a1 = getChar :: IO Char
-- read the current symbol from stdin; 'return' it;
-- change the World by making the next symbol in stding current.

-- What does getChar return? This depends on the state of the World,
-- and IS not a term in our program (every term is a 'constant').
-- So, we cannot test it this way:

-- test = (getChar == 'q') :: Bool

-- Notice that a1 is NOT the symbol returned but rather the
-- soubroutine "do read the current symbol!".

a2 = putChar :: Char -> IO ()
-- putChar 'q' is a subroutine to write the character 'q' into 
-- the current position in stdout and move to the next position;
-- the action putChar thus changes the world depending on the 
-- argument provided but returns nothing. 

-- We may combine these elementary actions into more involved
-- ones along the lines of imperative programming

-- read 3 chars but ignore the 2nd one
skip2nd :: IO (Char,Char)
skip2nd = do {x <- getChar; getChar; y <- getChar; return (x,y) }
-- x <- a mimics assingment of the value returned by the action a to
-- a local variable x, which may be an argument to further actions;
-- 'return' here is NOT what it might seem to be...

test_equal :: IO Bool
test_equal = do x <- getChar -- IO Char
                y <- getChar -- IO Char
-- another 'local variable' that 'purely' depends on the previous ones
                -- a term of type IO Bool starts
                let b = (x == y) in
                   do putStrLn $ if b then "\nyes" else "\nno" -- IO ()
                      return b -- IO Bool
                -- that term ends
                    
getLine' :: IO String
getLine' = do x <- getChar -- IO Char
              -- (
              if x == '\n' then
               return [] -- IO String
              else do xs <- getLine' -- IO String 
                      return (x:xs) -- IO String
              -- ) :: IO String

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = do putChar x
                    putStr' xs

-- What is under the hood of this 'do' construction?

-- (>>=) :: IO a -> (a -> IO b) -> IO b
-- (>>) :: IO a -> IO b -> IO b
-- return :: a -> IO a

-- These operators allow 'chaining' (or binding) actions
-- similarly to sequencing (;) operator of imperative languages.

a3 = getLine >>= putStrLn >> putStrLn "Hello!"

a3' = do s <- getLine
         putStrLn s
         putStrLn "Hello!"

-- Binding operators are left-associative;
-- but if we want to pass value resulting from an action
-- to all the further actions, we need a kind of right 
-- associativity

a4 = getLine >>= (\s -> putStrLn s >> putStrLn "This string reverted is:" >> putStrLn (reverse s))

-- This very behavior is implied by do-blocks:

a3'' = getLine >>= (\s -> putStrLn s >> putStrLn "Hello!")

a4' = do s <- getLine
         putStrLn s
         putStrLn "This string reverted is:"
         putStrLn $ reverse s
 
-- More generally:
myAction a1 a2 a3 a4 a5 a6 y =
     do x1 <- a1 y
        x2 <- a2 x1 y
        x3 <- a3 x1 x2 y
        a4 x3
        x5 <- a5 x2 x1
        a6 x3 x5

-- is equivalent to               
myAction' a1 a2 a3 a4 a5 a6 y =
    a1 y >>= (\x1 -> (a2 x1 y >>=
              (\x2 -> (a3 x1 x2 y >>=
               (\x3 -> (a4 x3 >>=
                (\_ -> (a5 x2 x1 >>= 
                 (\x5 -> a6 x3 x5)))))))))

-- What does 'return' stand for?
-- It allows to 'pack' a value into an action,
-- e.g., for passing this value to another action.

a5 = getLine >>= \s -> return (if even . length $ s then "even" else "odd")

a6 = a5 >>= putStrLn . reverse

-- Sequencing actions --
-- do actions from a list one by one 
sequence' :: [IO a] -> IO [a]
sequence' [] = return []
sequence' (z:zs) = do x <- z
                      xs <- sequence' zs                      
                      return (x:xs)
-- and collect the resulting values to a list

sequence'_ :: [IO a] -> IO ()
sequence'_ [] = return ()
sequence'_ (z:zs) = do z
                       sequence'_ zs                      
-- ...or discard those values.

-- print = putStrLn . show
f3 = sequence (map print [1,2,3,4,5])
f3' = sequence' (map print [1,2,3,4,5])
-- When 'evaluating' (in fact, 'doing') an action of type IO a,
-- ghci calls print (= putStrLn . show) for the value returned
-- unless a is ().
f3'' = sequence_ (map print [1,2,3,4,5])
f3''' = sequence'_ (map print [1,2,3,4,5])
-- mapM = sequence . map
f4 = mapM print [1,2,3,4,5]
f4' = mapM_ print [1,2,3,4,5]

-- Does not work as expected since >>= is
-- strict in its first argument for the IO monad.
-- (One can come across a few exceptions to this rule
-- for specific functions (like getContents), that perform
-- so-called unsafe IO operations in their implementaions.)
getLine'' :: IO String
getLine'' = sequence (repeat getChar) >>= return . takeWhile (/= '\n')
a7 = getLine'' >>= putStrLn

putStr'' :: String -> IO ()
putStr'' cs = sequence_ [ putChar c  | c <-cs ]

{-- Randomization --}

-- In pure code, 'random' numbers are strictly 
-- deterministic indeed. As in mathematics, it
-- is hard (whereas possible) to describe a particular
-- number as being 'random'. 

-- We rather define random generators than the values
-- they may produce...
gen = mkStdGen 30042018 

x0 = random gen :: (Int, StdGen)

x1 = random (snd x0) :: (Int, StdGen)

{--
-- next has been deprecated
x2 = next gen
x3 = next (snd $ next gen)
--}
(g1, g2) = split gen

y3 = random (gen) :: (Double, StdGen)
z3 = random (gen) :: (Bool, StdGen)

x4 = randomR ((-1),1) g1 :: (Int, StdGen)
x5 = randomR ((-1),1) g2 :: (Int, StdGen)

l1 = take 100 $ randomRs ((-1),1) g1 :: [Int]
l2 = take 100 $ randoms gen :: [Int]

mb0 = elem (fst x1) l2

l3 = take 16 $ randomRs ('A','z') g2 :: String

randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' g = x : randoms' g'
            where (x,g') = random g

l2' = take 100 $ randoms' gen :: [Int]

mb1 = l2 == l2'

-- To make generators practical, one needs to
-- reseed them with some volatile information 
-- (usually, system dependent).

a = do gen <- getStdGen
       putStrLn $ take 16 (randomRs ('A','z') gen)
       gen' <- getStdGen
       putStrLn $ take 16 (randomRs ('A','z') gen')


a' = do gen <- getStdGen
        putStrLn $ take 32 (randomRs ('A','z') gen)
        let (gen',gen'') = split gen
        putStrLn $ take 16 (randomRs ('A','z') gen')
        putStrLn $ take 16 (randomRs ('A','z') gen'')

ai = do gen <- getStdGen
        print $ take 32 (randomRs (1,10) gen :: [Int])
        let (gen',gen'') = split gen
        print $ take 16 (randomRs (1,10) gen' :: [Int])
        print $ take 16 (randomRs (1,10) gen'' :: [Int])



a'' = do gen <- newStdGen
         putStrLn $ take 16 (randomRs ('A','z') gen)
         gen' <- getStdGen
         putStrLn $ take 16 (randomRs ('A','z') gen')

-- reseeding and generating combined;
-- similar to C-style random()
x6 = randomIO :: IO Int
a''' = x6 >>= print

{-
 - for /dev/urandom based seeding, use initStdGen;
 - notice it doesn't update the global generator;
 - one can explicitly set it with setStdGen however.
 -}

a_ = do gen <- initStdGen
        putStrLn $ take 16 (randomRs ('A','z') gen)
        gen' <- getStdGen
        putStrLn $ take 16 (randomRs ('A','z') gen')


