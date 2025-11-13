{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
module Main where

import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

import Control.Monad.Identity
import Control.Monad.Except
import qualified Control.Monad.Reader as T
import qualified Control.Monad.Writer.Strict as T
import Control.Monad.State.Strict

main :: IO ()
main = putStrLn "Hello, Haskell!"

-- We want to interpret a small language of integer arithmetical expressions with variables
-- and lambdas

-- variable names (identifiers)
type Id = String

data Exp = Var Id | Cnst Int | Add Exp Exp | Sub Exp Exp
            | Mlt Exp Exp | Div Exp Exp | Mod Exp Exp
            | Abs Id Exp | App Exp Exp
    deriving (Read)

instance Show Exp where
    show e = case e of 
        Var x -> x
        Cnst n -> show n
        Add e1 e2 -> "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
        Sub e1 e2 -> "(" ++ show e1 ++ " - " ++ show e2 ++ ")"
        Mlt e1 e2 -> "(" ++ show e1 ++ " * " ++ show e2 ++ ")"
        Div e1 e2 -> "(" ++ show e1 ++ " / " ++ show e2 ++ ")"
        Mod e1 e2 -> "(" ++ show e1 ++ " % " ++ show e2 ++ ")"
        Abs x e1 -> "(λ " ++ x ++ " . " ++ show e1 ++ ")"
        App e1 e2 -> show e1 ++ " " ++ show e2

-- our expressions are to be valuated in a context which assigns values to (some) identifiers
type Ctx = M.Map Id Val

-- a value may be either an integer or a \lambda-term
data Val = IntVal Int | FunVal Ctx Id Exp

instance Show Val where
    show (IntVal n) = show n
    show (FunVal _ x e) = "λ " ++ x ++ " . " ++ show e ++ " (context not shown)"

-- a naive interpreting function:

eval0 :: Ctx -> Exp -> Val
eval0 ctx (Var x) = fromJust $ M.lookup x ctx -- may result in an exception
eval0 _ (Cnst c) = IntVal c
eval0 ctx (Add e1 e2) = let IntVal v1 = eval0 ctx e1; IntVal v2 = eval0 ctx e2 -- ditto; you can't add a lambda to a number;
                        in IntVal $ v1 + v2                                    -- yet we have no types to prevent a try
eval0 ctx (Sub e1 e2) = let IntVal v1 = eval0 ctx e1; IntVal v2 = eval0 ctx e2 -- ditto
                        in IntVal $ v1 - v2
eval0 ctx (Mlt e1 e2) = let IntVal v1 = eval0 ctx e1; IntVal v2 = eval0 ctx e2 -- ditto
                        in IntVal $ v1 * v2 
eval0 ctx (Div e1 e2) = let IntVal v1 = eval0 ctx e1; IntVal v2 = eval0 ctx e2 -- ditto
                        in IntVal $ v1 `div` v2
eval0 ctx (Mod e1 e2) = let IntVal v1 = eval0 ctx e1; IntVal v2 = eval0 ctx e2 -- ditto
                        in IntVal $ v1 `mod` v2 
-- we store the context for the case of nested lambda (i.e., partial function application);
-- (\x -> (\y -> x + y)) a b evaluates to (\y -> x + y) b (we don't want inefficient substitutions!),
-- but the information that x := a must be stored somewhere!
eval0 ctx (Abs x e) = FunVal ctx x e
eval0 ctx (App e1 e2) = let FunVal ctx' x' e' = eval0 ctx e1 -- ditto
                            v2 = eval0 ctx e2
                        in eval0 (M.insert x' v2 . M.union ctx' $ ctx) e'

-- a few examples on how this works

empty_ctx = M.empty
ctx1 = M.fromList [("a", IntVal 14),("zero", IntVal 0),("b", IntVal (-3)),("w", IntVal 5),
                    ("fun1", FunVal empty_ctx "x" (Add (Var "x") (Cnst 8))),
                    ("fun2", FunVal empty_ctx "x" (Mlt (Var "x") (Var "w"))),
                    ("omega2", FunVal empty_ctx "x" (App (Var "x")(Var "x")))]

exp0 = Add (Cnst 12) (App (Abs "z" (Mod (Var "z")(Cnst 5))) (Mlt (Cnst 7) (Cnst 4)) )
exp1 = Mlt (Var "a") (Cnst 4)
exp2 = Div exp1 (Var "zero")
exp3 = App (Var "fun1") (Sub (Cnst 11) exp1)
exp4 = App (Abs "y" (App (Var "y") (Cnst 5))) (Var "fun1")
exp5 = App (Var "omega2") (Abs "z" (Var "z"))
_Omega = App (Var "omega2") (Var "omega2")
-- cannot add a number to a lambda
exp6 = Add (Cnst 1) exp5

-- with (eval0 (M.insert x' v2 ctx') e') at the end of eval0 definition,
-- evaluating exp7 in context ctx1 fails since the value ctx1 "w" will never be
-- propagate into the stored context of fun2's body (which is set to be empty by ctx1) 
exp7 = App (Var "fun2") (Cnst 2)
exp8 = App (Abs "w" exp7) (Cnst 6)

exp9 = Abs "a" (Mlt (Var "a") (Var "a"))
exp10 = Add (Var "a") (App exp9 (Cnst 5))

-- This might be seen as a design flaw (say, why store the context with FunVal at all?),
-- be it not the case of...

-- Partial application!
exp11 = App (Abs "b" (Abs "a" (Add (Var "a") (Var "b")))) (Cnst 1)
exp12 = App (App (Abs "b" (Abs "a" (Add (Var "a") (Var "b")))) (Cnst 1)) (Cnst 2)

-- Yet there is a solution that combines our stored context ctx' with the current context ctx,
-- so that the former will be preferred: we can use M.union. Of course, this is less effecient.

-- so many possibilities for an error... so much this context stuff...
-- Can monads help us here? Let us give them a chance!

-- Error handling

-- we need this instance for the case when our patterns fail (i.e., when the value obtained is 
-- of a 'wrong type'); for Maybe we have one in base libraries but maybe would give us just Nothing
-- without any meaningful error message.
instance MonadFail (Either String) where
--    fail :: String -> Either String a
    fail = Left

-- the only required law is: fail s >>= f = fail s (much like mzero/mempty in MonadPlus)

-- recall that return = Right for (Either a) monad.
eval1 :: Ctx -> Exp -> Either String Val
eval1 ctx (Var x) = maybe (Left $ "undefined variable: " ++ x) return $  M.lookup x ctx 
eval1 _ (Cnst c) = return $ IntVal c
-- of course, we can separate matching a pattern from 'assigning a value';
-- this way we could call fail (or Left) explicitly; we shall take this
-- approach later
eval1 ctx (Add e1 e2) = do IntVal v1 <- eval1 ctx e1; IntVal v2 <- eval1 ctx e2 
                           return . IntVal $ v1 + v2                           
eval1 ctx (Sub e1 e2) = do IntVal v1 <- eval1 ctx e1; IntVal v2 <- eval1 ctx e2 
                           return . IntVal $ v1 - v2
eval1 ctx (Mlt e1 e2) = do IntVal v1 <- eval1 ctx e1; IntVal v2 <- eval1 ctx e2 
                           return . IntVal $ v1 * v2
-- we should make some provisions for division by zero
eval1 ctx (Div e1 e2) = do IntVal v1 <- eval1 ctx e1; IntVal v2 <- eval1 ctx e2 
                           fmap IntVal $ if (v2 == 0) then Left "division by zero" else return $ v1 `div` v2
eval1 ctx (Mod e1 e2) = do IntVal v1 <- eval1 ctx e1; IntVal v2 <- eval1 ctx e2 
                           fmap IntVal $ if (v2 == 0) then Left "division by zero" else return $ v1 `mod` v2
eval1 ctx (Abs x e) = return $ FunVal ctx x e
eval1 ctx (App e1 e2) = do FunVal ctx' x' e' <- eval1 ctx e1
                           v2 <- eval1 ctx e2
                           eval1 (M.insert x' v2 . M.union ctx' $ ctx) e'

-- Now we treat all possible errors much nicer 

-- Hiding context

-- The need to mention ctx everywhere in our definition makes it somewhat clumsy.
-- Can we put it to some 'state' and ask for it on as-needed basis?
-- Surely! Moreover, here we do not need the full power of the State monad (which
-- essentailly contains 'state updaters' of type s -> (s,a)). It suffice to
-- have s -> a only, which is the essense of Reader s a monad.

{- Reader monad -}
-- Of course, we could import it from Control.Monad.Reader
newtype Reader s a = Reader { runReader :: s -> a }

instance Functor (Reader s) where
   fmap = liftM

instance Applicative (Reader s) where
-- pure :: a -> Reader s a
   pure x = Reader $ \_ -> x
   (<*>) = ap

instance Monad (Reader s) where
-- (>>=) :: Reader s a -> (a -> Reader s b) -> Reader s b
   mx >>= f = Reader $ \st -> let x = runReader mx st
                               in runReader (f x) st

-- a reader (cf. class MonadReader) should support the following functions:

-- read the state
ask :: Reader a a
ask = Reader id

-- apply a function to the state read
asks :: (s -> a) -> Reader s a
asks f = Reader f 

-- run a computation for a modified state; notice that the latter is a 
-- 'new local copy' of the state; the state does not change for the
-- computations further in sequnce.
local :: (s -> s) -> Reader s a -> Reader s a
local f mx = Reader $ runReader mx . f

-- so, our new version of eval will be...

-- But what can we do with error handling?..

-- One ugly measure is obvious:
instance MonadFail (Reader Ctx) where
-- Reader has no capacity to store error information.
    fail s = error $ 
        "This exception is necessary to report the error: " ++ s

eval2' :: Exp -> Reader Ctx Val
eval2' (Var x) = do ctx <- ask
                    maybe (fail $ "undefined varaible: " ++ x) return $  M.lookup x ctx 
eval2' (Cnst c) = return $ IntVal c
eval2' (Add e1 e2) = do IntVal v1 <- eval2' e1; IntVal v2 <- eval2' e2 
                        return . IntVal $ v1 + v2                           
eval2' (Sub e1 e2) = do IntVal v1 <- eval2' e1; IntVal v2 <- eval2' e2 
                        return . IntVal $ v1 - v2
eval2' (Mlt e1 e2) = do IntVal v1 <- eval2' e1; IntVal v2 <- eval2' e2 
                        return . IntVal $ v1 * v2
eval2' (Div e1 e2) = do IntVal v1 <- eval2' e1; IntVal v2 <- eval2' e2 
                        fmap IntVal $ if (v2 == 0) then (fail "division by zero") else return $ v1 `div` v2
eval2' (Mod e1 e2) = do IntVal v1 <- eval2' e1; IntVal v2 <- eval2' e2 
                        fmap IntVal $ if (v2 == 0) then (fail "division by zero") else return $ v1 `mod` v2
eval2' (Abs x e) = do ctx <- ask
                      return $ FunVal ctx x e
eval2' (App e1 e2) = do FunVal ctx' x' e' <- eval2' e1
                        v2 <- eval2' e2
                        ctx <- ask
                        local (const $ M.insert x' v2 . M.union ctx' $ ctx) (eval2' e')

eval2 :: Ctx -> Exp -> Val
eval2 ctx e = runReader (eval2' e) ctx

-- we have better code but worse error handling now. :-(

{- Writer monad -}

-- Let us try a kind of logging of our evaluation. One natural way do it is the
-- Writer monad. Unlike s -> (s,a) of the State monad, it is essentially just (s,a).

newtype Writer s a = Writer { runWriter :: (a,s) }

instance Functor (Writer s) where
-- this would require s to be a monoid, which is not needed indeed
--    fmap = liftM
-- fmap :: (a -> b) -> Writer s a -> Writer s b
    fmap f (Writer (x, st)) = Writer (f x, st)

instance Monoid s => Applicative (Writer s) where
    pure x = Writer (x, mempty)
    (<*>) = ap

instance Monoid s => Monad (Writer s) where
    mx >>= f = Writer $
     let (x, st) = runWriter mx
         (x', st') = runWriter (f x)
     in (x', st <> st')

-- obtaining the state
execWriter :: Writer s a -> s
execWriter = snd . runWriter

-- write to the state directly
tell :: s -> Writer s ()
tell st = Writer ((), st)

-- So, our logging-enabled eval function is...
instance MonadFail (Writer [String]) where
-- similarly to Reader, Writer has no 'error state', however it is able
-- to store an error message.
    fail s = Writer (error "There was an error. Read the log!", [s])

myTell :: Exp -> Val -> Writer [String] Val
myTell e v = tell ["evaluating " ++ show e ++ " to " ++ show v] >> return v 

eval3' :: Ctx -> Exp -> Writer [String] Val
eval3' ctx e = case e of
    Var x ->  maybe (fail $ "undefined variable " ++ x ++ " in " ++ show e) 
                        (myTell e) $  M.lookup x ctx 
    Cnst c -> myTell e $ IntVal c
    Add e1 e2 -> do IntVal v1 <- eval3' ctx e1; IntVal v2 <- eval3' ctx e2 
                    myTell e . IntVal $ v1 + v2                           
    Sub e1 e2 -> do IntVal v1 <- eval3' ctx e1; IntVal v2 <- eval3' ctx e2 
                    myTell e . IntVal $ v1 - v2
    Mlt e1 e2 -> do IntVal v1 <- eval3' ctx e1; IntVal v2 <- eval3' ctx e2 
                    myTell e . IntVal $ v1 * v2
    Div e1 e2 -> do IntVal v1 <- eval3' ctx e1; IntVal v2 <- eval3' ctx e2 
                    if (v2 == 0) then (fail $ "division by zero in " ++ show e) else myTell e . IntVal $ v1 `div` v2
    Mod e1 e2 -> do IntVal v1 <- eval3' ctx e1; IntVal v2 <- eval3' ctx e2 
                    if (v2 == 0) then (fail $ "division by zero in " ++ show e) else myTell e . IntVal $ v1 `mod` v2
    Abs x e1 -> myTell e $ FunVal ctx x e1
    App e1 e2 -> do FunVal ctx' x' e' <- eval3' ctx e1
                    v2 <- eval3' ctx e2
                    eval3' (M.insert x' v2 . M.union ctx' $ ctx) e' >>= myTell e

eval3 :: Ctx -> Exp -> IO ()
eval3 ctx e = do let (v,ss) = runWriter $ eval3' ctx e
                 putStrLn "Log: " >> putStr (unlines ss)  
                 putStr "Value: " >> print v

-- So far, we have seen a few effects we can augment our evaluation with.
-- But what if we want to combine some of them to get their benefits together?
-- This is just the point where monad transformers are usually called in.

-- First, let us have some theory.

-- class MonadTrans (t :: (* -> *) -> * -> *) where
--  lift :: Monad m => m a -> (t m) a

-- A monad transformer t is a function on unary type functions such that a function lift
-- is defined. Intuitively, t 'injects' a new monad into a monadic computation m x so that
-- it may have a new effect in addition to that of m. Of course, we want to do it so that
-- t m is a monad as well. In particular, one has:
--   MaybeT m a \cong m (Maybe a)
--   ReaderT s m a \cong Reader s (m a) \cong s -> m a
--   WriterT s m a \cong m (Writer s a) \cong m (a, s)
--   StateT s m a \cong Reader s m (Writer s a) \cong s -> m (a, s)

-- Notice that, say, x :: MaybeT m a DOES NOT mean that x :: m (Maybe a);
-- it is rather x :: m' a for some new monad m'. If x :: MaybeT [] Int, the
-- following code will compile:
x1 = return 5 :: MaybeT [] Int
--x' = return (Just 5) :: MaybeT [] Int
x2 = lift [2,3] :: MaybeT [] Int
x3 = lift [] :: MaybeT [] Int
-- notice the constructor MaybeT
x4 = MaybeT [Just 5, Nothing, Just 3]
y = x4 >>= return . (+1)

-- the function lift applies a new t-effects to a computation in m;
-- it is required that
--  lift . return = return
--  lift (mx >>= f) = lift mx >>= (lift . f)

-- As the Identity monad 'does nothing', one has
-- ReaderT s Identity a \cong Reader s a, and so on.
-- This way it is possible to define many monads via the respective
-- transformers.

-- ExceptT handles errors (it does not throw IO-exceptions generally, 
-- as one can make the 'exception' type e arbitrary):
-- ExceptT e m a \cong m (Either e a)

type Eval4 a = ExceptT String Identity a

-- What are the kinds of ExceptT, ExceptT String, ExceptT String Identity 
-- (= Eval4)? What should reasonably be a monad?

runEval4 :: Eval4 a -> Either String a
runEval4 = runIdentity  . runExceptT

eval4' :: Ctx -> Exp -> Eval4 Val
eval4' ctx (Var x) = maybe (throwError $ "undefined variable: " ++ x) return $  M.lookup x ctx 
eval4' _ (Cnst c) = return $ IntVal c
eval4' ctx (Add e1 e2) = do v1' <- eval4' ctx e1; v2' <- eval4' ctx e2
                            case (v1',v2') of
                                (IntVal v1, IntVal v2) -> return . IntVal $ v1 + v2
                                _ -> throwError "type error in addition"                          
eval4' ctx (Sub e1 e2) = do v1' <- eval4' ctx e1; v2' <- eval4' ctx e2
                            case (v1',v2') of
                                (IntVal v1, IntVal v2) -> return . IntVal $ v1 - v2
                                _ -> throwError "type error in subtraction"                          
eval4' ctx (Mlt e1 e2) = do v1' <- eval4' ctx e1; v2' <- eval4' ctx e2
                            case (v1',v2') of
                                (IntVal v1, IntVal v2) -> return . IntVal $ v1 * v2
                                _ -> throwError "type error in multiplication"                          
eval4' ctx (Div e1 e2) = do v1' <- eval4' ctx e1; v2' <- eval4' ctx e2
                            case (v1',v2') of
                                (IntVal v1, IntVal v2) | v2 /= 0 -> return . IntVal $ v1 `div` v2
                                                       | otherwise -> throwError "division by zero"
                                _ -> throwError "type error in division"                          
eval4' ctx (Mod e1 e2) = do v1' <- eval4' ctx e1; v2' <- eval4' ctx e2
                            case (v1',v2') of
                                (IntVal v1, IntVal v2) | v2 /= 0 -> return . IntVal $ v1 `mod` v2
                                                       | otherwise -> throwError "division by zero"
                                _ -> throwError "type error in division"                          
eval4' ctx (Abs x e) = return $ FunVal ctx x e
eval4' ctx (App e1 e2) = do v1 <- eval4' ctx e1
                            v2 <- eval4' ctx e2
                            case v1 of
                                (FunVal ctx' x' e') -> eval4' (M.insert x' v2 . M.union ctx' $ ctx) e'
                                _ -> throwError "type error in application"

eval4 :: Ctx -> Exp -> Either String Val
eval4 ctx exp = runEval4 $ eval4' ctx exp

-- so far we had just one effect and no need to lift our computations.

-- Let us add Reader.
 
type Eval5 a = T.ReaderT Ctx (ExceptT String Identity) a
runEval5 :: Ctx -> Eval5 a -> Either String a
runEval5 ctx eval = runEval4 $ T.runReaderT eval ctx

eval5' :: Exp -> Eval5 Val
eval5' e = case e of
-- In this do-block, the type of ask must be Eval5 Val = ReaderT Ctx m' Val =
-- m'' Val. According to the type signature of ask, the monad m'' must be in
-- the MonadReader Ctx class. You may run :info MonadReader to see the many
-- instances of this class that guarantee m'' to be therein even if we change
-- the position of ReaderT in our chain of transformers (see also below).
    Var x -> do ctx <- T.ask
-- Similarly, throwError gives us something of the type m' Val with m' belonging to
-- the class MonadError String, while it must give us Eval5 Val = ReaderT ...
-- Inspecting the output of :info MonadError, one can see that there is an instance
-- of MonadError Strings for ReaderT r m given m belongs to MonadError String itself.
-- As our m is ExceptT ..., it belongs to this class naturally indeed.
-- Be there no such abundance of 'cross'-instances for standard monads, we would
-- have to use lift a lot, by, say, lifting throwError x :: (ExceptT ...) a to
-- (ReaderT r (ExceptT ...)) a (look at the type signature for lift again).  
                maybe (throwError $ "undefined variable " ++ x ++ " in " ++ show 3)
                    return $  M.lookup x ctx
 
    Cnst c -> return $ IntVal c

    Add e1 e2 -> do v1' <- eval5' e1; v2' <- eval5' e2
                    case (v1',v2') of
                      (IntVal v1, IntVal v2) -> return . IntVal $ v1 + v2
                      _ -> throwError $ "type error in addition in " ++ show e
                          
    Sub e1 e2 -> do v1' <- eval5' e1; v2' <- eval5' e2
                    case (v1',v2') of
                      (IntVal v1, IntVal v2) -> return . IntVal $ v1 - v2
                      _ -> throwError $ "type error in subtraction in " ++ show e
                          
    Mlt e1 e2 -> do v1' <- eval5' e1; v2' <- eval5' e2
                    case (v1',v2') of
                      (IntVal v1, IntVal v2) -> return . IntVal $ v1 * v2
                      _ -> throwError $ "type error in multiplication in " ++ show e
                          
    Div e1 e2 -> do v1' <- eval5' e1; v2' <- eval5' e2
                    case (v1',v2') of
                      (IntVal v1, IntVal v2) | v2 /= 0 -> return . IntVal $ v1 `div` v2
                                             | otherwise -> throwError $
                                                 "division by zero in " ++ show e
                      _ -> throwError $ "type error in division in " ++ show e

    Mod e1 e2 -> do v1' <- eval5' e1; v2' <- eval5' e2
                    case (v1',v2') of
                      (IntVal v1, IntVal v2) | v2 /= 0 -> return . IntVal $ v1 `mod` v2
                                             | otherwise -> throwError $
                                                 "division by zero in " ++ show e
                      _ -> throwError $ "type error in division in " ++ show e
                   
    Abs x e -> do ctx <- T.ask
                  return $ FunVal ctx x e

    App e1 e2 -> do v1 <- eval5' e1
                    v2 <- eval5' e2
                    ctx <- T.ask
                    case v1 of
                        (FunVal ctx' x' e') -> T.local (M.insert x' v2 . M.union ctx') (eval5' e')
                        _ -> throwError $ "type error in application in " ++ show e

eval5 :: Ctx -> Exp -> Either String Val
eval5 ctx e = runEval5 ctx (eval5' e)

-- Let us add Writer.

-- Notice its position in the chain: we want to apply it after ExceptT (that is,
-- ExceptT must take it as its monadic argument) for we will effectively return
-- Either String (Writer [String] Val) instead of Writer [String] (Either String Val)
-- otherwise, that is, we will have a log just when there is no error.
type Eval6 a = T.ReaderT Ctx (ExceptT String (T.WriterT [String] Identity)) a
runEval6 :: Ctx -> Eval6 a -> (Either String a, [String])
runEval6 ctx = runIdentity . T.runWriterT . runExceptT . flip T.runReaderT ctx

myTell' :: (T.MonadWriter [String] m) => Exp -> Val -> m Val
myTell' e v = T.tell ["evaluating " ++ show e ++ " to " ++ show v] >> return v

-- assume we want to copy error messages to the log
myThrow s = T.tell [s] >> throwError s

eval6' :: Exp -> Eval6 Val
eval6' e = case e of
    Var x -> do ctx <- T.ask
                maybe (myThrow $ "undefined variable " ++ x ++ " in " ++ show e)
                     (myTell' e) $  M.lookup x ctx

    Cnst c -> return $ IntVal c

    Add e1 e2 -> do v1' <- eval6' e1; v2' <- eval6' e2
                    case (v1',v2') of
                      (IntVal v1, IntVal v2) -> myTell' e . IntVal $ v1 + v2
                      _ -> myThrow $ "type error in addition in " ++ show e
                          
    Sub e1 e2 -> do v1' <- eval6' e1; v2' <- eval6' e2
                    case (v1',v2') of
                      (IntVal v1, IntVal v2) -> myTell' e . IntVal $ v1 - v2
                      _ -> myThrow $ "type error in subtraction in " ++ show e
                          
    Mlt e1 e2 -> do v1' <- eval6' e1; v2' <- eval6' e2
                    case (v1',v2') of
                      (IntVal v1, IntVal v2) -> myTell' e . IntVal $ v1 * v2
                      _ -> myThrow $ "type error in multiplication in " ++ show e
                          
    Div e1 e2 -> do v1' <- eval6' e1; v2' <- eval6' e2
                    case (v1',v2') of
                      (IntVal v1, IntVal v2) | v2 /= 0 -> myTell' e . IntVal $ v1 `div` v2
                                             | otherwise -> myThrow $
                                                 "division by zero in " ++ show e
                      _ -> myThrow $ "type error in division in " ++ show e

    Mod e1 e2 -> do v1' <- eval6' e1; v2' <- eval6' e2
                    case (v1',v2') of
                      (IntVal v1, IntVal v2) | v2 /= 0 -> myTell' e . IntVal $ v1 `mod` v2
                                             | otherwise -> myThrow $
                                                 "division by zero in " ++ show e
                      _ -> myThrow $ "type error in division in " ++ show e
                   
    Abs x e -> do ctx <- T.ask
                  myTell' e $ FunVal ctx x e

    App e1 e2 -> do v1 <- eval6' e1
                    v2 <- eval6' e2
                    ctx <- T.ask
                    case v1 of
                        (FunVal ctx' x' e') -> T.local (M.insert x' v2 . M.union ctx') (eval6' e')
                                                >>= myTell' e
                        _ -> myThrow $ "type error in application in " ++ show e


-- we can 'exctract' the results from our monad
eval6'' :: Ctx -> Exp -> (Either String Val, [String])
eval6'' ctx e = runEval6 ctx (eval6' e)

-- ...and can pretty-print them then in the IO monad:
eval6 :: Ctx -> Exp -> IO ()
eval6 ctx e = do let (v',ss) = eval6'' ctx e
                 putStrLn "Log: " >> putStr (unlines ss)
                 case v' of 
                    Right v -> putStr "Value: " >> print v
                    Left s  -> putStr "Error: " >> putStrLn s

-- But what if we want do some IO *during* evaluation? Say, we want to allow the user
-- to correct "undefined variable" errors.

-- With ReaderT, the user has to ascribe a value to *every* free occurence of an undefined
-- variable; he can, of course, provide different values to such occurences. We will use
-- StateT instead in order to 'memorize' the user's first choice for each variable.

-- As a matter of fact, there is NO IOT transformer able to 'inject' IO-effects into the
-- context of an arbitrary monad. The common practice is to place IO instead of Identity
-- to the bottom of the monad 'stack'. The run* function will then lack runIdetity's last call
-- and will return an IO-action.

type Eval7 a = StateT Ctx (ExceptT String (T.WriterT [String] IO)) a
runEval7 :: Ctx -> Eval7 a -> IO (Either String a, [String])
-- we have evalStateT instead of runStateT since we need no state finally
runEval7 ctx = T.runWriterT . runExceptT . flip evalStateT ctx

readVal :: IO [Val]
readVal = do putStrLn "please type a number or a correct Abs expression; type STOP to quit"
             putStrLn $ "example: " ++ show 42
             putStrLn $ "example: " ++ "Abs \"x\" (Var \"x\")"
             s <- getLine
             if (s == "STOP") then
                return []
-- reads maps a string to a list of possible parses
             else
                do let p_int = (reads s :: [(Int,String)])
                   if not . null $ p_int then
                    return . (:[]) . IntVal . fst . head $ p_int
                   else
                    do let p_fun = reads s :: [(Exp,String)]
                       if null p_fun then
                        do putStrLn "no parse; please try again"
                           readVal
                       else
                        do case fst . head $ p_fun of  
                            (Abs x e) -> return . (:[]) $ FunVal empty_ctx x e
                            _ -> do putStrLn "no parse; please try again"
                                    readVal

eval7' :: Exp -> Eval7 Val
eval7' e = case e of
    Var x -> do ctx <- get
                let v' = M.lookup x ctx
                case v' of
                    Just v -> myTell' e v
-- we NEED an explicit lift (i.e., liftIO or lift . lift . lift alternatively)
-- in order to make the type of our action correct; this is possible for any
-- stack of transformers given IO lies at its bottom; this lift is neccessary
-- as there is no (practical) way to instantiate all possible IO 'methods' for
-- every trnsformer (as the mtl library does for every pair like
-- (ReaderT, Writer) etc.).
                    Nothing -> do vs <- liftIO $ (putStrLn ("undefined variable " ++ x ++ " in " ++ show e) >>
                                         readVal)
                                  case vs of
                                       (v:_) -> modify (M.insert x v) >> myTell' e v
                                       _ -> myThrow $ "undefined variable " ++ x ++ " in " ++ show e

    Cnst c -> return $ IntVal c

    Add e1 e2 -> do v1' <- eval7' e1; v2' <- eval7' e2
                    case (v1',v2') of
                      (IntVal v1, IntVal v2) -> myTell' e . IntVal $ v1 + v2
                      _ -> myThrow $ "type error in addition in " ++ show e
                          
    Sub e1 e2 -> do v1' <- eval7' e1; v2' <- eval7' e2
                    case (v1',v2') of
                      (IntVal v1, IntVal v2) -> myTell' e . IntVal $ v1 - v2
                      _ -> myThrow $ "type error in subtraction in " ++ show e
                          
    Mlt e1 e2 -> do v1' <- eval7' e1; v2' <- eval7' e2
                    case (v1',v2') of
                      (IntVal v1, IntVal v2) -> myTell' e . IntVal $ v1 * v2
                      _ -> myThrow $ "type error in multiplication in " ++ show e
                          
    Div e1 e2 -> do v1' <- eval7' e1; v2' <- eval7' e2
                    case (v1',v2') of
                      (IntVal v1, IntVal v2) | v2 /= 0 -> myTell' e . IntVal $ v1 `div` v2
                                             | otherwise -> myThrow $
                                                 "division by zero in " ++ show e
                      _ -> myThrow $ "type error in division in " ++ show e

    Mod e1 e2 -> do v1' <- eval7' e1; v2' <- eval7' e2
                    case (v1',v2') of
                      (IntVal v1, IntVal v2) | v2 /= 0 -> myTell' e . IntVal $ v1 `mod` v2
                                             | otherwise -> myThrow $
                                                 "division by zero in " ++ show e
                      _ -> myThrow $ "type error in division in " ++ show e
                   
    Abs x e -> do ctx <- get
                  myTell' e $ FunVal ctx x e

    App e1 e2 -> do v1 <- eval7' e1
                    v2 <- eval7' e2
                    ctx <- get
                    case v1 of
                        (FunVal ctx' x' e') -> do modify (M.insert x' v2 . M.union ctx')
                                                  v0 <- eval7' e'
-- restore the state as we may get out of this lambda
                                                  put ctx
                                                  myTell' e v0
                        _ -> myThrow $ "type error in application in " ++ show e

-- pretty-printing
eval7'' :: Ctx -> Exp -> IO (Either String Val, [String])
eval7'' ctx e = runEval7 ctx (eval7' e)

eval7 :: Ctx -> Exp -> IO ()
eval7 ctx e = do (v',ss) <- eval7'' ctx e
                 putStrLn "Log: " >> putStr (unlines ss)
                 case v' of 
                    Right v -> putStr "Value: " >> print v
                    Left s  -> putStr "Error: " >> putStrLn s

-- In practice, the RWST transformer may be used to combine reading, writing and 'updating'
-- three (independent) 'states'.

------------------------------------------------------------------------------------------------------

-- Does this stuff look complicated? It likely does, as it *is* complicated.
-- There are alternative systems (effectful, Bluefin,...) to (more or less) explicitly handle various
-- effects. And these systems are complicated also! Moreover, this 'effect accounting' comes at an
-- efficiency price.

-- So, we'd better keep things simple and do as much as it is possible in pure functional code,
-- which is as nice as mathematics can be.


