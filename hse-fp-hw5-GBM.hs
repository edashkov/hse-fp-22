import System.Random

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

-- return [] if x*x + y*y > 1;
-- getting x*x + y*y = 0.0 has (theoretical) probability 0
transform :: (Floating a, Ord a) => (a, a) -> [a]
transform (x,y) | s < 1.0 = [x * t, y * t]
                | otherwise = []
                where s = x * x + y * y
                      t = sqrt (-2.0 * log s / s)

-- the Box-Muller tranform maps a pair of independent uniform variables to a pair of independent standard normal variables
boxMuller :: IO [Float]
boxMuller =  do rpairs <- newStdGen >>= return . uncurry zip . both (randomRs (-1::Float, 1::Float)) . split
                return $ foldMap transform rpairs

-- drift and volatility are given for the period (100% = 1.0); say, (-0.05) and 0.25 may be realistic annual values;
-- timestep is a fraction of the period;
-- 252 * 390 = trading minutes per year;
-- one minute as the timestep = 1.0 / (252*390); 
-- one day as the timestep = 1.0 / 252;

geomBM init drift vol timestep rands  = scanl diff init rands
        where diff s rand = s * (a + b * rand)
              a = 1.0 + drift * timestep
              b = vol * sqrt timestep

-- Example: > fmap (take 100) $ geomBMIO 100 0.0 0.2 (1.0/252)
-- simulates a 100-day 'trendless' market with annual volatility 20%
geomBMIO init drift vol timestep = boxMuller >>= return . geomBM init drift vol timestep   

