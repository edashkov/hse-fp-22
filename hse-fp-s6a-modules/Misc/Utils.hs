-- export everything
module Misc.Utils where
-- the dots in the module's name mimic the path to it

catPairs :: [(a,a)] -> [a]
-- clearly, this behavior is wanting;
-- so, we may consider this a 'testing'
-- version of the function
catPairs _ = []

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y) 

