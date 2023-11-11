{-# LANGUAGE StandaloneDeriving #-}
-- we need this not most of the time

import Data.Function

{-- Type Classes --}

-- What is the type of (==)?

{--
class Eq a  where
    (==), (/=)           :: a -> a -> Bool

    x /= y               = not (x == y)
    x == y               = not (x /= y)
    {-# MINIMAL (==) | (/=) #-}
--}

data MyType = Foo | Bar

instance Eq MyType where
 Foo == Foo = True
 Bar == Bar = True
 _ == _ = False

{--
class  (Eq a) => Ord a  where
    compare              :: a -> a -> Ordering
    (<), (<=), (>), (>=) :: a -> a -> Bool
    max, min             :: a -> a -> a

    compare x y = if x == y then EQ
                  else if x <= y then LT
                  else GT

    x <  y = case compare x y of { LT -> True;  _ -> False }
    x <= y = case compare x y of { GT -> False; _ -> True }
    x >  y = case compare x y of { GT -> True;  _ -> False }
    x >= y = case compare x y of { LT -> False; _ -> True }

    max x y = if x <= y then y else x
    min x y = if x <= y then x else y
    {-# MINIMAL compare | (<=) #-}

--}

instance Ord MyType where
    Bar <= Foo = False
    _ <= _ = True

{--

class Show a where
    show :: a -> String

class Read a where
    read :: String -> a

-- In fact, defining pretty-printing is more involved
--    and parsing is much more so.

--}

instance Show MyType where
    show Foo = "Foo'"
    show Bar = "Bar'"

-- Let us set parsing on automagically
deriving instance Read MyType

m1 = read "Foo" :: MyType
 
{--

class  Enum a   where
    -- | the successor of a value.  For numeric types, 'succ' adds 1.
    succ                :: a -> a
    -- | the predecessor of a value.  For numeric types, 'pred' subtracts 1.
    pred                :: a -> a
    -- | Convert from an 'Int'.
    toEnum              :: Int -> a
    -- | Convert to an 'Int'.
    -- It is implementation-dependent what 'fromEnum' returns when
    -- applied to a value that is too large to fit in an 'Int'.
    fromEnum            :: a -> Int

    -- | Used in Haskell's translation of @[n..]@ with @[n..] = enumFrom n@,
    --   a possible implementation being @enumFrom n = n : enumFrom (succ n)@.
    --   For example:
    --
    --     * @enumFrom 4 :: [Integer] = [4,5,6,7,...]@
    --     * @enumFrom 6 :: [Int] = [6,7,8,9,...,maxBound :: Int]@
    enumFrom            :: a -> [a]
    -- | Used in Haskell's translation of @[n,n'..]@
    --   with @[n,n'..] = enumFromThen n n'@, a possible implementation being
    --   @enumFromThen n n' = n : n' : worker (f x) (f x n')@,
    --   @worker s v = v : worker s (s v)@, @x = fromEnum n' - fromEnum n@ and
    --   @f n y
    --     | n > 0 = f (n - 1) (succ y)
    --     | n < 0 = f (n + 1) (pred y)
    --     | otherwise = y@
    --   For example:
    --
    --     * @enumFromThen 4 6 :: [Integer] = [4,6,8,10...]@
    --     * @enumFromThen 6 2 :: [Int] = [6,2,-2,-6,...,minBound :: Int]@
    enumFromThen        :: a -> a -> [a]
    -- | Used in Haskell's translation of @[n..m]@ with
    --   @[n..m] = enumFromTo n m@, a possible implementation being
    --   @enumFromTo n m
    --      | n <= m = n : enumFromTo (succ n) m
    --      | otherwise = []@.
    --   For example:
    --
    --     * @enumFromTo 6 10 :: [Int] = [6,7,8,9,10]@
    --     * @enumFromTo 42 1 :: [Integer] = []@
    enumFromTo          :: a -> a -> [a]
    -- | Used in Haskell's translation of @[n,n'..m]@ with
    --   @[n,n'..m] = enumFromThenTo n n' m@ 
    --   For example:
    --
    --     * @enumFromThenTo 4 2 -6 :: [Integer] = [4,2,0,-2,-4,-6]@
    --     * @enumFromThenTo 6 8 2 :: [Int] = []@
    enumFromThenTo      :: a -> a -> a -> [a]

    succ = toEnum . (+ 1) . fromEnum

    pred = toEnum . (subtract 1) . fromEnum

    enumFrom x = map toEnum [fromEnum x ..]

    enumFromThen x y = map toEnum [fromEnum x, fromEnum y ..]

    enumFromTo x y = map toEnum [fromEnum x .. fromEnum y]

    enumFromThenTo x1 x2 y = map toEnum [fromEnum x1, fromEnum x2 .. fromEnum y]

    {-# MINIMAL (fromEnum, toEnum) #-}

--}

instance Enum MyType where
    toEnum 1 = Foo
    toEnum 2 = Bar
    toEnum _ = error "out of range"

    fromEnum Foo = 1
    fromEnum Bar = 2

m2 = [(Foo)..]
m3 = [(Foo)..(Bar)]
m4 = [(Bar)..(Foo)]

m5 = [2..]
m6 = [2,5..]


{-- Numeric types --}

{--

class  Num a  where
    {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-}

    (+), (-), (*)       :: a -> a -> a
    -- | Unary negation.
    negate              :: a -> a
    -- | Absolute value.
    abs                 :: a -> a
    -- | Sign of a number.
    -- The functions 'abs' and 'signum' should satisfy the law:
    --
    -- > abs x * signum x == x
    --
    -- For real numbers, the 'signum' is either @-1@ (negative), @0@ (zero)
    -- or @1@ (positive).
    signum              :: a -> a
    -- | Conversion from an 'Integer'.
    -- An integer literal represents the application of the function
    -- 'fromInteger' to the appropriate value of type 'Integer',
    -- so such literals have type @('Num' a) => a@.
    fromInteger         :: Integer -> a

    x - y               = x + negate y
    negate x            = 0 - x

-- | the same as @'flip' ('-')@.
--
-- Because @-@ is treated specially in the Haskell grammar,
-- @(-@ /e/@)@ is not a section, but an application of prefix negation.
-- However, @('subtract'@ /exp/@)@ is equivalent to the disallowed section.
subtract :: (Num a) => a -> a -> a
subtract x y = y - x

--}

instance Num MyType where
    Foo + Foo = Foo
    Bar + Foo = Bar
    Foo + Bar = Bar
    Bar + Bar = Foo
    _ * Foo = Foo
    Foo * _ = Foo
    Bar * Bar = Bar
    negate = id
    abs = id
    signum = id
    fromInteger n = if mod n 2 == 0 then Foo else Bar

m7 = Bar * (Foo + Bar)

{--

class  (Num a, Ord a) => Real a  where
    -- | the rational equivalent of its real argument with full precision
    toRational          ::  a -> Rational

class  (Real a, Enum a) => Integral a  where
    -- | integer division truncated toward zero
    --
    -- WARNING: This function is partial (because it throws when 0 is passed as
    -- the divisor) for all the integer types in @base@.
    quot                :: a -> a -> a
    -- | integer remainder, satisfying
    --
    -- > (x `quot` y)*y + (x `rem` y) == x
    --
    -- WARNING: This function is partial (because it throws when 0 is passed as
    -- the divisor) for all the integer types in @base@.
    rem                 :: a -> a -> a
    -- | integer division truncated toward negative infinity
    --
    -- WARNING: This function is partial (because it throws when 0 is passed as
    -- the divisor) for all the integer types in @base@.
    div                 :: a -> a -> a
    -- | integer modulus, satisfying
    --
    -- > (x `div` y)*y + (x `mod` y) == x
    --
    -- WARNING: This function is partial (because it throws when 0 is passed as
    -- the divisor) for all the integer types in @base@.
    mod                 :: a -> a -> a
    -- | simultaneous 'quot' and 'rem'
    --
    -- WARNING: This function is partial (because it throws when 0 is passed as
    -- the divisor) for all the integer types in @base@.
    quotRem             :: a -> a -> (a,a)
    -- | simultaneous 'div' and 'mod'
    --
    -- WARNING: This function is partial (because it throws when 0 is passed as
    -- the divisor) for all the integer types in @base@.
    divMod              :: a -> a -> (a,a)
    -- | conversion to 'Integer'
    toInteger           :: a -> Integer

    n `quot` d          =  q  where (q,_) = quotRem n d
    n `rem` d           =  r  where (_,r) = quotRem n d
    n `div` d           =  q  where (q,_) = divMod n d
    n `mod` d           =  r  where (_,r) = divMod n d

    divMod n d          =  if signum r == negate (signum d) then (q-1, r+d) else qr
                           where qr@(q,r) = quotRem n d

-- for a field
class  (Num a) => Fractional a  where
    {-# MINIMAL fromRational, (recip | (/)) #-}

    -- | Fractional division.
    (/)                 :: a -> a -> a
    -- | Reciprocal fraction.
    recip               :: a -> a
    -- | Conversion from a 'Rational' (that is @'Ratio' 'Integer'@).
    -- A floating literal stands for an application of 'fromRational'
    -- to a value of type 'Rational', so such literals have type
    -- @('Fractional' a) => a@.
    fromRational        :: Rational -> a

    recip x             =  1 / x
    x / y               = x * recip y


--}



{-- Algebraic Data Types  --}

{-- Synonym is NOT what we want --}
type MyType1 = MyType

--deriving instance Eq MyType1

type Assoc k v = [(k,v)]

find :: Eq k =>  Assoc k v -> k -> v
find [] x = error "Key not found"
find ((z,z') : zs) x | z == x        = z'
                     | otherwise     = find zs x


{-- Finite sets --}

data Void 

data Singleton = Singleton
    deriving (Eq,Show)

-- data () = ()

data Day = Monday
          | Tuesday | Wednesday
          | Thursday | Friday
          | Saturday | Sunday
          deriving (Eq,Ord,Show,Read)

today = Monday
b1 = Tuesday < Friday

next :: Day -> Day
next x = case x of
            Monday -> Tuesday
            Tuesday -> Wednesday
            Wednesday -> Thursday
            Thursday -> Friday
            Friday -> Saturday
            Saturday -> Sunday
            Sunday -> Monday

comp 0 f = id
comp n f = f . (comp (n - 1) f)

prev :: Day -> Day
prev = comp 6 next

d1 = next today
d2 = prev today

-- Bool, Ordering

{-- Records --}

data Date = Dt (Int,Int,Int)
        deriving (Eq)

d3 = Dt (2,4,2018)
d4 = Dt (2,4,2017)

b2 = d3 == d4
b3 = d3 == d3

instance Show Date where
    show (Dt (d,m,y)) = (if d < 10 then "0" else "")
                      ++ show d ++ "."
                      ++ (if m < 10 then "0" else "")
                      ++ show m ++ "." ++ show y

s1 = show d3

-- Clearly, Dt :: (Int,Int,Int) -> Date might be curried to
-- Dt :: Int -> Int -> Int -> Date. But would it be more natural?

-- An alterenative style for multi-field records:
-- the accessor functions are then defined automatically.
data Date'' = Dt'' { day :: Int, month :: Int, year :: Int }
              deriving (Eq,Show)

d101 = Dt'' 16 4 2019
m101 = month d101
d102 =  Dt'' {year = 1861, day = 11, month = 02}

--

-- newtype X' = C X creates a 'run-time' synonym type for X:
-- it is separate from X for the compiler but then constructor
-- C is lifted for the sake of optimization.
newtype Date' = Dt' (Int,Int,Int)
                deriving (Eq,Show)

d5 = Dt' (2,4,2018)

-- Just one construtor for newtypes:
--newtype Color = Red Int | Green Int | Blue Int

-- Just one argument for that constructor:
--newtype Color = RGB Int Int Int

-- So, one could just uncurry it:
newtype Color = RGB (Int,Int,Int)


-- Parametric records

data Triplet a b c = Triplet a b c

--data (,,) a b c = (a,b,c)

m8 = Triplet 8 "Product" [1,2,3]

-- the most primitive product type ("record")

-- "a * b"
--data (,) a b = (a,b)

m8a = (2,'2') :: (Int,Char)

{-- Sum types --}

-- every finite set is a sum...
-- 2 = 1 + 1
-- data Bool = True | False

data NumsNchars = Number Int | Character Char
    deriving (Eq,Ord,Show)

m9 = Number 17
m10 = Character 'U'
-- in general, one cannot compare an Int to a Char.
m11 = m9 == m10

-- one can easily take a sum of products in ONE type definition:

data SumProd a b c = Prod1 a b | Prod2 c a  

-- "a + 1"
-- data Maybe a = Nothing | Just a

sdiv :: Int -> Int -> Maybe Int
sdiv n m | m /= 0       = Just $ n `div` m
         | otherwise    = Nothing


-- maybe :: b -> (a -> b) -> Maybe a -> b

-- squaring "safe" numbers to an Int value
ssquare :: Maybe Int -> Int
ssquare = maybe (-1) (^2)

m12 = ssquare $ sdiv 24 7
m13 = ssquare $ sdiv 24 0

-- the most primitive form of summation
-- "a + b"
--data Either a b = Left a  | Right b

-- either :: (a -> c) -> (b -> c) -> Either a b -> c

-- one recommended way of error handling
-- when the arguments are 'just right' the result is a Right ...
sdiv' :: Int -> Int -> Either String Int
sdiv' n m | m /= 0       = Right $ n `div` m
          | otherwise    = Left "division by zero"

ssquare' :: Either String Int -> Int
ssquare' = either (\_ -> (-1)) (^2)

-- But how can one extract the values? How can one manipulate 'packed' values?
-- Apparently, one needs some type-transforming machinery in order to do so effectively. 

{-- Recursive types --}
data Nat = Z | S Nat
        deriving (Eq,Ord,Show)

-- It is NOT true that Nat is the LEAST set of terms
-- that contains Z and S t for its every member t.
-- In fact, Nat is the GREATEST set that contains
-- t whenever it contains S t.
-- That's why Nat contains the following term:
infty = S infty 
-- Indeed, consider the set X = {infty, S infty}. It is closed
-- in the latter sense, whence X is a subset on Nat and infty :: Nat.
-- The term infty is MORE than just 'undefined', for it is indeed 'infinite'

infty' = S infty'

-- how can one prove these to 'infinities' be the same?
mb0 = (infty == infty')

n1 = S (S (S Z))
n2 = S (S Z)

-- {Z == Z = True; Z == S _ = False; S _ == Z = False; S x == S y = x == y)
-- S x <= S y = x <= y
b4 = n2 < n1

int2nat :: (Integral a) => a -> Nat
int2nat n | n < 0       = error "negative value"
          | n == 0      = Z
          | otherwise   = S (int2nat (n - 1))

nadd :: Nat -> Nat -> Nat
nadd m Z  = m
nadd m (S n) = S (nadd n m)


n3 = nadd n1 n2

nat2int :: (Integral a) => Nat -> a
nat2int Z = 0
nat2int (S n) = (nat2int n) + 1

-- foldn transforms every natural to an iterator
-- fold f x (S (S (S Z))) = f (f (f x))
foldn :: (a -> a) -> a -> Nat -> a
foldn f x Z = x
foldn f x (S n) = f (foldn f x n)


nat2int' :: Nat -> Int
nat2int' = foldn (+1) 0

nadd' = foldn S
m14 = nadd' (int2nat 5)

-- our 'infinity' term can be translated, say, into an
-- inifinite list via foldn, so it is genuinely infinite:
m14a = foldn (1:) [] infty


-- Cf. the iterative construction for prdecessor 
-- from the untyped lambda calculus
npred = snd . foldn (\(n,_) -> (S n, n)) (Z,Z)

nmlt m =  snd . (foldn (\(l, y) -> (l, nadd y l)) (m,Z))

-- direct iteration pure lambda style
nmlt' n m = foldn (foldn S m) Z n

nfac = snd . foldn (\(n,x) -> (S n, nmlt (S n) x )) (Z, S Z)


instance Num Nat where
    n + m = nadd n m
    n * m = nmlt n m
    negate _ = Z
    signum Z = Z
    signum (S _) = S Z
    abs = id
    fromInteger = int2nat

-- notice the use of literals; it is due to fromInteger, abs and negate
n5 = (n1 * n2 * 12) + (nfac n3) + (-3)


{-- Parameters --}

-- kinds of Bool etc., Maybe, Either, (,), (->).
-- parametric families of types = polymorphic types = type transformers
-- some 'nice' transformers will be later defined as functors, etc.

-- there is ONE recursive definition for all types in a family
-- (if you don't like it, try TypeFamilies extension):
data Tree a = Nil | Node (Tree a) a (Tree a)
        deriving (Eq,Show)

myTree = Node (Node Nil 'f' Nil) 'o' (Node Nil 'o' Nil)

{--
-- we still have infinite objects of various forms
-- as Tree a is the GREATEST term set closed under
-- the rule: if Node l x r \in X, then l \in X and r \in X.

-- The first example
-- X1 = { myTree, Node Nil 'f' Nil, Node Nil 'o' Nil, Nil }
-- proves X1 \subset Tree Char and myTree :: Tree Char,
-- while the fact that
-- X2 = {Node Nil 1 infBranchedR, Nil, infBranchedR} is
-- closed guarantees infBranchedR :: Tree Int
-- (the type must be the same due to the type signature of Node;
-- also notice that we still need a defininig equation for
-- infBranchedR unlike Nil (which is defined by data Nat)).
--}

infBranchedR = Node Nil 1 infBranchedR
infBranchedL = Node infBranchedL 1 Nil

-- 'mutual coinduction'
inf1 = Node inf2 3 inf1
inf2 = Node inf1 5 inf2

mb1 = infBranchedR == infBranchedL
mb2 = infBranchedR == infBranchedR

ms1 = take 100 $ show infBranchedR
ms2 = take 100 $ show infBranchedL

-- lists are of utmost practical importance in Haskell
-- infixr :
-- data [a] = [] | a : [a]

-- a type clearly isomorphic to lists:
-- (infixr specifies :+ as infix and right-associative)
infixr :+
-- an infix constructor must begin with :
data MyL a = Nl | a :+ (MyL a)

flatten :: Tree a -> [a]
flatten Nil = []
flatten (Node left x right) = flatten left ++ [x] ++ flatten right

ml0 = flatten myTree
ml1 = flatten infBranchedR

-- recursive polymorphic types are very powerful;
-- e.g., Y-combinator is typeable with them:

myY :: (a -> a) -> a

-- notice the negative occurence of Rec in the
-- constructor's type. You may not do this
-- trick in Coq.
newtype Rec a = MkRec (Rec a -> a)
getRec (MkRec f) = f

-- Indeed, MkRec works miracles: it maps an extractor
-- from Rec a to Rec a. This is like being able to
-- transform a procedure of extracting $100 from a
-- wallet that contains that much (I am sure you have
-- such a procedure) to such a wallet.

-- the polymorphic functions MkRec and getRec provide
-- an isomorphism between Rec a -> a and Rec a for any a.

myY = \f -> let g = \x -> f (getRec x x) in 
                g (MkRec g)

-- one can also implement typeable combinators similar
-- to \omega_2 and \Omega
omega_2 = \x -> getRec x x
_Omega = omega_2 (MkRec omega_2)
_Omega' = myY id

-- this combinator is usable
myFac = myY $ \fac n -> if n <= 0 then 1 else n * fac (n - 1)
m15 = myFac 4

-- cf. fix from Data.Function
myFac' = fix $ \fac n -> if n <= 0 then 1 else n * fac (n - 1)

-- How does it work?
-- generally, myY phi equals such x that x = phi x,
-- since phi (myY phi) = myY phi

-- (2:) :: [Int] -> [Int]
-- the infinite list of 2's
-- myY (2:) -> 2 : (myY (2:)) -> 2 : 2 : (my (2:)) -> ...
m16 = myY (2:)
-- is the same as
m16' = (2:) m16'

-- S :: Nat -> Nat
-- the 'infinite natural':
infty'' = myY S


-- if there are many such x's, not the one we like 
-- may be computed: say, myY (*3) diverges and does
-- not return 0.

-- if a is a funtional type, one can halt iterations using an argument:
-- \f -> (\n -> if n == 0 then 1 else 2 * (f $ n - 1)) :: (Int -> Int) -> (Int -> Int)
m17 = myY (\f -> (\n -> if n == 0 then 1 else 2 * (f $ n - 1))) $ 5
-- let g = \f -> (\n -> if n == 0 then 1 else 2 * (f $ n - 1))
-- myY g 2 -> g (myY g) 2 -(lazy evaluation!)-> 2 * (myY g 1) -> 2 * (g (myY g) 1) ->
--  -> 2 * (2 * (myY g 0)) -> 2 * (2 * (g (myY g) 0)) -> 2 * (2 * 1) -> 4

-- What do we have with types?

-- rf is just 'packed' f of type Rec (Int -> Int) 
pow2' :: Rec (Int -> Int) -> (Int -> Int) 
-- so, pow2' takes a 'recursive call' of some function
-- as its first argument and returns a function.
-- (notice the 'self-applications' here)
pow2' = \rf -> (\n -> if n == 0 then 1 else 2 * ( (getRec rf) rf (n - 1) ))

pow2 :: Int -> Int
-- pow2 is just pow2 'applied to itself'
pow2 = pow2' $ MkRec pow2'

-- in principle, one may apply pow2' to other functions
-- of type Rec (Int -> Int) -> (Int -> Int)
m18 = pow2' $ MkRec (\_ -> id)
m19 = pow2' $ MkRec (\_ -> (+3))
-- ignoring the first argument results in no actual recursion

infList' :: Rec [Int] -> [Int]
-- rl :: Rec [Int]
infList' = \rl -> 2 : ((getRec rl) rl)

infList :: [Int]
infList = infList' $ MkRec infList'

