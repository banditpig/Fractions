-- Why does flatten $ 1  + F 1 (F 1 (2 + F 6 35)) give a list of
-- just one fraction? ([111/35])
-- Answer. flatten $ 1  + F 1 (F 1 (2 + F 6 35)) does the '+' which gives
-- a single fraction.
-- Doing flatten $ F 1 (F 1 (2 + F 6 35)) gives a list and the last entry is 'resolved'
-- ie [1/1,1/1,76/35].
-- To get a list of the fractions generated for, say, root2 do
-- fracs  = [root2 n | n <- [1..10]] or however many...


module Fractions where
import           Data.Ratio

type Numerator = Integer
type Denominator = Integer
{-
So a fraction can be a simple
3
3/4
or
3 / (4 / (...))
or a recursive, continued, fraction:
-}
data Fraction  = Numbr Numerator | F Numerator Fraction

num :: Fraction -> Integer
num (Numbr n) = n
num (F n _)   = n

denom :: Fraction -> Integer
denom (Numbr _)       = 1
denom (F _ (Numbr n)) = n
denom (F _ f@(F _ _)) = denom f


evalFrac :: Fraction -> Float
evalFrac f = fromIntegral n / fromIntegral d where
    F n (Numbr d) = reduce . simplify $ f

instance Show Fraction where
    show (Numbr n) =  show n ++ "/1"
    show (F n  0) = show n ++ "/0" -- " Error. Division by 0 is undefined!"
    show (F n (Numbr d))
        | n == 0 = show 0
        | d == 1 = show n ++ "/1"
        | (n > 0 && d < 0) || (n < 0 && d < 0 ) = show (-n) ++ "/" ++ show (-d)
        | otherwise = show n ++ "/" ++ show d
    show (F n f@(F _ _)) = show n ++  "/" ++  show f


mul :: Fraction -> Fraction -> Fraction
mul f f' = mul' (simplify f) (simplify f') where
    mul' (Numbr p) (Numbr q)               = Numbr (p * q)
    mul' (Numbr p) (F p' (Numbr q'))       = F (p * p') (Numbr q')
    mul' (F p' (Numbr q')) (Numbr p)       = F (p * p') (Numbr q')
    mul' (F p (Numbr q)) (F p' (Numbr q')) = F (p * p') (Numbr (q * q'))


add :: Fraction -> Fraction -> Fraction
add f f' = add' (simplify f) (simplify f') where
    add' (Numbr p) (Numbr q)               = Numbr (p + q)
    add' (Numbr p) (F p' (Numbr q'))       = F (q' * p + p') (Numbr q')
    add' (F p' (Numbr q')) (Numbr p)       = F (q' * p + p') (Numbr q')
    add' (F p (Numbr q)) (F p' (Numbr q')) = F (p * q' + q * p') (Numbr (q * q'))
    -- (Numbr _) (F _ (F _ _))
    -- (F _ (Numbr _)) (F _ (F _ _))
    -- (F _ (F _ _)) _

sub :: Fraction -> Fraction -> Fraction
sub f f' = sub' (simplify f) (simplify f') where
    sub' (Numbr p) (Numbr q)               = Numbr (p - q)
    sub' (Numbr p) (F p' (Numbr q'))       = F (q' * p - p') (Numbr q')
    sub' (F p' (Numbr q')) (Numbr p)       = F (q' * p - p') (Numbr q')
    sub' (F p (Numbr q)) (F p' (Numbr q')) = F (p * q' - q * p') (Numbr (q * q'))


divid :: Fraction -> Fraction -> Fraction
divid f f' = divid' (simplify f) (simplify f') where
    divid' (Numbr p) (Numbr q)               = F p (Numbr q)
    divid' (Numbr p) (F p' (Numbr q'))       = F (p * q') (Numbr p')
    divid' (F p' (Numbr q')) (Numbr p)       = F p' (Numbr (q' * p) )
    divid' (F p (Numbr q)) (F p' (Numbr q')) = F (p * q') (Numbr (q * p'))

simplify :: Fraction -> Fraction
simplify f = foldr (/) lastFraction remainingFractions where
    flat = flatten f
    lastFraction = last flat
    remainingFractions = takeWhile (/= lastFraction) flat

simplify' :: Fraction -> Fraction -> Fraction
-- these are fractions that are p/q i.e q is not a recurring fraction
simplify' (Numbr p) (Numbr q)               = F p (Numbr q)
simplify' (Numbr p) (F p' (Numbr q) )       = F (p * q) (Numbr p')
simplify' (F p' (Numbr q) ) (Numbr p)       = F (p * p') (Numbr 1)
-- Note A : simplify' (F p' (Numbr q) ) (Numbr p)       = F p' (Numbr (p * q))
simplify' (F p (Numbr p')) (F q (Numbr q')) = F (p * q')  (Numbr (p' * q))

-- eg reduce 6/10 -> 3/5, reduce 3/5 -> 3/5
reduce :: Fraction -> Fraction
reduce (Numbr n) = Numbr n
reduce f@(F n (Numbr d))
    | gDiv == 1 = f
    | otherwise = F  (n `div` gDiv) (Numbr (d `div` gDiv)) where
        gDiv = gcd n d
reduce (F n f) = F n (reduce f)

instance Eq Fraction where
    (==) f g = res where
         fr  = reduce f
         gr  = reduce g
         res = num fr == num gr && denom fr == denom gr

instance Ord Fraction where
    (<=) f g = signum (f - g ) == -1

instance Num Fraction where
    -- {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-}
    (+) = add
    (*) = mul
    (-) = sub
    abs (Numbr n) = Numbr (abs n)
    abs f@(F n d)   = F  (abs n') (abs d') where F n' d' = simplify f
    signum (Numbr n) = Numbr (signum n)
    signum f@(F n d)
        | n' == 0 =  Numbr 0
        | n' > 0 && d' < 0 = Numbr (-1)
        | n' < 0 && d' > 0 = Numbr (-1)
        | otherwise = Numbr 1 where
            F n' d' = simplify f
    fromInteger = Numbr

instance Fractional Fraction where
    --{-# MINIMAL fromRational, (recip | (/)) #-}
    fromRational x = F (numerator x) (Numbr (denominator x))
    (/) = divid

-- Take a possibly recursive fraction and reduce it to a list of fractions
flatten :: Fraction -> [Fraction]
flatten f@(F _ (Numbr _) ) = [f]
flatten (Numbr n)          = [F n (Numbr 1)]
flatten (F n f)            = Numbr n : flatten f


contFrac :: (Integer -> Fraction) -> (Integer -> Numerator) -> Integer -> Fraction
contFrac fa fb  = rf 0  where
      rf n t
        | n > t = 0
        | otherwise =  fa n + F (fb n) (rf (n + 1) t)


root2 :: Integer -> Fraction
root2  = contFrac fa fb where
    fa 0 = 1
    fa _ = 2
    fb _ = 1

root5 :: Integer -> Fraction
root5  = contFrac fa fb  where
                fa 0 = 2
                fa _ = 4
                fb _ = 1
phi :: Integer -> Fraction
phi  = contFrac fa fb  where
                fa _ = 1
                fb _ = 1


zG = 2 + F 1 (1 + F 1 (4 + F 1 3))
zB = 4 + F 1 (3 + F 1 2)
z1 = 2 + F 1 ( 1 + F 1 ( 3 + F 1 4))

