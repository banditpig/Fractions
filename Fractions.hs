module Fractions where
import           Data.Ratio

type Numerator = Integer
type Denominator = Integer
{-
So a fraction can be a simple
3 or 3/4 or 3 / (4 / (...))
or a recursive, continued, fraction:
-}
data Fraction  = Numbr Numerator | F Numerator Fraction

instance Show Fraction where
    show (Numbr n) =  show n ++ "/1"
    show (F n  0) = show n ++ "/0"
    show (F n (Numbr d))
        | n == 0 = show 0
        | d == 1 = show n ++ "/1"
        | (n > 0 && d < 0) || (n < 0 && d < 0 ) = show (-n) ++ "/" ++ show (-d)
        | otherwise = show n ++ "/" ++ show d
    show (F n f@(F _ _)) = show n ++  "/" ++  show f

-- Make Fraction into a Number so that +, * etc can be used.
instance Num Fraction where
    (+) = add
    (*) = mul
    (-) = sub
    abs (Numbr n) = Numbr (abs n)
    abs f@(F n d)   = F  (abs n1) (abs d1) where F n1 d1 = simplify f
    signum (Numbr n) = Numbr (signum n)
    signum f@(F n d)
        | n1 == 0 =  Numbr 0
        | n1 > 0 && d1 < 0 = Numbr (-1)
        | n1 < 0 && d1 > 0 = Numbr (-1)
        | otherwise = Numbr 1 where
            F n1 d1 = simplify f
    fromInteger = Numbr

-- Allows the / operator to be used with Fraction
instance Fractional Fraction where
    fromRational x = F (numerator x) (Numbr (denominator x))
    (/) = divid

-- Notion of equality between Fractions
instance Eq Fraction where
    (==) f g = res where
            fr  = reduce f
            gr  = reduce g
            res = num fr == num gr && denom fr == denom gr

-- And ordering
instance Ord Fraction where
    (<=) f g = signum (f - g ) == -1

-- Fraction specific definitions of basic arithmetic ops. These are referenced
-- in the previous typeclass definitions.
mul :: Fraction -> Fraction -> Fraction
mul (F 0 0 ) f  = f
mul f (F 0 0 )  = f
mul f f1 = mul1 (simplify f) (simplify f1) where
    mul1 (Numbr p) (Numbr q)               = Numbr (p * q)
    mul1 (Numbr p) (F p1 (Numbr q1))       = F (p * p1) (Numbr q1)
    mul1 (F p1 (Numbr q1)) (Numbr p)       = F (p * p1) (Numbr q1)
    mul1 (F p (Numbr q)) (F p1 (Numbr q1)) = F (p * p1) (Numbr (q * q1))

add :: Fraction -> Fraction -> Fraction
add (F 0 0 ) f  = f
add f (F 0 0 )  = f
add f f1 = add1 (simplify f) (simplify f1) where
    add1 (Numbr p) (Numbr q)               = Numbr (p + q)
    add1 (Numbr p) (F p1 (Numbr q1))       = F (q1 * p + p1) (Numbr q1)
    add1 (F p1 (Numbr q1)) (Numbr p)       = F (q1 * p + p1) (Numbr q1)
    add1 (F p (Numbr q)) (F p1 (Numbr q1)) = F (p * q1 + q * p1) (Numbr (q * q1))

sub :: Fraction -> Fraction -> Fraction
sub (F 0 0 ) f  = f
sub f (F 0 0 )  = f
sub f f1 = sub1 (simplify f) (simplify f1) where
    sub1 (Numbr p) (Numbr q)               = Numbr (p - q)
    sub1 (Numbr p) (F p1 (Numbr q1))       = F (q1 * p - p1) (Numbr q1)
    sub1 (F p1 (Numbr q1)) (Numbr p)       = F (q1 * p - p1) (Numbr q1)
    sub1 (F p (Numbr q)) (F p1 (Numbr q1)) = F (p * q1 - q * p1) (Numbr (q * q1))

divid :: Fraction -> Fraction -> Fraction
divid f f1 = divid1 (simplify f) (simplify f1) where
    divid1 (Numbr p) (Numbr q)               = F p (Numbr q)
    divid1 (Numbr p) (F p1 (Numbr q1))       = F (p * q1) (Numbr p1)
    divid1 (F p1 (Numbr q1)) (Numbr p)       = F p1 (Numbr (q1 * p) )
    divid1 (F p (Numbr q)) (F p1 (Numbr q1)) = F (p * q1) (Numbr (q * p1))

-- This works by taking a possibly nested fraction, flattening
-- it into a list and then folds (/) over the list to simplify the
-- fraction
simplify :: Fraction -> Fraction
simplify f = foldr (/) lastFraction remainingFractions where
    flat = flatten f
    lastFraction = last flat
    remainingFractions = takeWhile (/= lastFraction) flat


-- Remove common divisors
-- eg reduce 6/10 -> 3/5, reduce 3/5 -> 3/5
reduce :: Fraction -> Fraction
reduce (Numbr n) = Numbr n
reduce f@(F n (Numbr d))
    | gDiv == 1 = f
    | otherwise = F  (n `div` gDiv) (Numbr (d `div` gDiv)) where
        gDiv = gcd n d
reduce (F n f) = F n (reduce f)

-- Take a possibly recursive fraction and reduce it to a list of fractions
flatten :: Fraction -> [Fraction]
flatten f@(F _ (Numbr _) ) = [f]
flatten (Numbr n)          = [F n (Numbr 1)]
flatten (F n f)            = Numbr n : flatten f


-- Simple pattern matching to get the numerator from a Fraction
num :: Fraction -> Integer
num (Numbr n) = n
num (F n _)   = n

-- and the denominator
denom :: Fraction -> Integer
denom (Numbr _)       = 1
denom (F _ (Numbr n)) = n
denom (F _ f@(F _ _)) = denom f

-- Takes a Fraction, simplifies it by a "flatten and foldr" technique,
-- removes common divisors and finally resolves the fraction as a Float.
evalFrac :: Fraction -> Float
evalFrac f = fromIntegral n / fromIntegral d where
    F n (Numbr d) = reduce . simplify $ f

--           The 'a' coefficients     The 'b' coefficients     Depth
contFrac :: (Integer -> Fraction) -> (Integer -> Numerator) -> Integer -> Fraction
contFrac fa fb  = rf 0  where
      rf n t
        | n > t = 0
        | otherwise =  fa n + F (fb n) (rf (n + 1) t)


-- Takes n/d and returns the CF in list form
--    41/13 = [3; 6, 2] = [3; 6, 1, 1]
--    124/37 = [3; 2, 1, 5, 2] = [3; 2, 1, 5, 1, 1]
--    5/12 = [0; 2, 2, 2] = [0; 2, 2, 1, 1]
toCF :: Integer -> Integer -> [Integer]
toCF n d
    | d' == 0 = [a]
    | otherwise =  a : toCF d d' where
       (a, d') = divMod n d

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


