--The State monad and friends --

import Control.Monad.State
--import Control.Monad.Writer

-- let us develop a simplistic stack calculator for Int values
-- supporting just push, pop, add, and mult operations

-- How can one model a stack? Using lists is an obvious choice.

type Stack = [Int]

{-- Every stack operation may
    (1) transform the stack;
    (2) return a value, or () as a 'no value' placeholder;
    (3) take a numerical argument.

The NAIVE approach is to make the stack an explicit argument of those:
--}

push' :: Int -> Stack -> ((), Stack)
push' x = \st -> ((), x:st)

pop' :: Stack -> (Int, Stack)
pop' = \(x:xs) -> (x, xs)

add' ::  Stack -> (Int, Stack)
add' = \(x:y:xs) -> (x + y, (x + y):xs)

mult' ::  Stack -> (Int, Stack)
mult' = \(x:y:xs) -> (x * y, (x * y):xs)


-- let us compute (3 + 5) * (17 - 11) with these stack operations; 
-- in Polish notation, we have * + 3 5 + 17 * (-1) 11, whence:
comp1 :: Int
comp1 = let (_,st1) = push' 11 []    -- [11]
            (_,st2) = push' (-1) st1 -- [-1, 11]
            (_,st3) = mult' st2      -- [-11]
            (_,st4) = push' 17 st3   -- [17, -11]
            (_,st5) = add' st4       -- [6]
            (_,st6) = push' 5 st5    -- [5, 6]
            (_,st7) = push' 3 st6    -- [3, 5, 6]
            (_,st8) = add' st7       -- [8, 6]
            (_,st9) = mult' st8 in   -- [48]
        fst . pop' $ st9

-- We have lots of 'boilerplate' code here and an obvious
-- challenge of 'chaining' the opertaions via the temporary
-- variable st1,..,st9. Can we call monads in? 

-- Surely!

-- all our operations' types follow the pattern:
-- a1 -> a2 -> ... -> an -> Stack -> (b, Stack)
-- Notice the head type: Stack -> (b, Stack);
-- this has a very clear meaning of 'change the stack and return b,
-- which is similar to the imaginery type World -> (b, World)
-- we used to explain IO b...

-- In a word, we have a type for operations that may change some 'global',
-- not immediately accessible 'variable' or 'state' (be it Stack or World).


-- This idea has got abstracted into

-- newtype State s a = State { runState :: (s -> (a, s)) }

-- IO a is thus roughly equivalent State World a.

-- In fact, the definition in Control.Monad.State differs for it is based
-- on the monad transformer StateT capable of augmenting any monad with
-- state-like behavior. Instead of the constructor State, it provides

-- state :: (s -> (a, s)) -> State s a

-- What are the kinds of State, State Stack?

{--

instance Monad (State s) where
-- return :: a -> m a
-- return :: a -> State s a
    return x = state $ \s -> (x,s)

-- (>>=) :: m a -> (a -> m b) -> m b
-- (>>=) :: State s a -> (a -> State s b) -> State s b

t >>= f = \st -> let (x', st')  = runState t st 
                    in runState (f x') st'

--}

-- Now, the stack state is implicit for our opertaions

push :: Int -> State Stack ()
push x = state $ \st -> ((), x:st)

pop :: State Stack Int
pop = state $ \(x:xs) -> (x, xs)

add ::  State Stack Int
add = state $ \(x:y:xs) -> (x + y, (x + y):xs)

mult ::  State Stack Int
mult = state $ \(x:y:xs) -> (x * y, (x * y):xs)

-- Our computation now looks much more natural 
-- and close to an imperative-style stack manipulation

comp2' :: State Stack Int
comp2' = do push 11 
            push (-1)
            mult 
            push 17
            add 
            push 5 
            push 3 
            add
            mult
            pop

-- How can we get the resulting value?

-- evalState :: State s a -> s -> a
-- evalState t st = fst (runState t st)
comp2 :: Int
comp2 = evalState comp2' []

-- How can we get the resulting state (i.e., stack)?

-- execState :: State s a -> s -> s
-- execState t st = snd (runState t st)
comp2stack :: Stack
comp2stack = execState comp2' []

-- Clearly, every object of type State Stack b is a stack
-- transformer with some value returned.

-- We can easily combine such stack transformers using
-- monad mechanics. 

-- How can we make a tranformation start from some given state?

-- put :: s -> State s ()
-- put st = state $ \_ -> ((), st)
emptyStack = put [] :: State Stack ()
myStack = put [6,-8,3] :: State Stack ()

-- Obtain the current state as a value.
-- get :: State s s
-- get = state $ \st -> (st, st)

-- Explicitely modify the state.
-- modify :: (s -> s) -> State s ()
-- modify f = state $ \st -> ((), f s)

-- compute (x + y^2) `mod` z for a stack (x:y:z:_);
-- keep the stack as (z:_)
comp3 = do x <- pop -- (y:z:_)
           y <- pop -- (z:_)
           z <- pop -- (_)
           push z   -- (z:_)
           push y   -- (y:z:_)
           push y   -- (y:y:z:_)
           mult     -- (y^2:z:_)
           push x   -- (x:y^2:z:_)
           add      -- (x + y^2:z:_)
           w <- pop -- (z:_) 
           return $ w `mod` z
           
           
comp4 = myStack >> modify reverse >> comp3 >>= (\x -> modify (x:)) >> get
comp4' = execState comp4 $ repeat 0
  
--- It is now possible to rewrite our stack ops in more uniform fashion:

push'' :: Int -> State Stack ()
push'' x = modify (x:)

pop'' :: State Stack Int
pop'' = get >>= (\(x:xs) -> put xs  >> return x)

add'' ::  State Stack ()
add'' = do x <- pop''
           y <- pop''
           push'' $ x + y 

mult'' ::  State Stack ()
mult'' = do x <- pop''
            y <- pop''
            push'' $ x * y 

-- As an example, we consider translating a ground arithmetical term
-- (like (2 + 3)*(5*(7+2))) into a stack transformer. Such a task
-- is natural for compiler development.

data Tm = Val Int | Add Tm Tm | Mlt Tm Tm

eval' :: Tm -> State Stack ()
eval' (Val n) = push'' n
-- compute the first argument first
-- init_stack --> val(t1) : init_stack --> val(t2) : val (t1) : init_stack --> val(t2) + val(t1) : init_stack
eval' (Add t1 t2) = eval' t1 >> eval' t2 >> add''
eval' (Mlt t1 t2) = eval' t1 >> eval' t2 >> mult''

-- eval' t is an abstract representation of the term's t semantics

eval :: Tm -> Int
eval t = evalState  (eval' t >> pop'') []

-- (9 + (2 * 3)) *(4 + (-6))
myTm1 = Mlt (Add (Val 9) (Mlt (Val 2) (Val 3))) (Add (Val 4) (Val (-6)))   


---
--- Writer Monad
---

-- While State s a is essetially s -> (a,s), we may be sometimes 
-- happy with just (a,s) -- where the state is updated but never read
-- (like it is typical in logging)

-- TODO
