module Fractions where
import           Data.Monoid
import           Data.Ratio
import           Debug.Trace
import           Numeric

type Numerator = Integer
type Denominator = Integer
data Fraction  = Numbr Numerator | F Numerator Fraction

formatFloatN floatNum numOfDecimals = showFFloat (Just numOfDecimals) floatNum ""
float20  flt =  formatFloatN flt 10

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

    show (Numbr n) =  show n
    show (F _  0) = "Error division by 0 is undefined!"
    show (F n (Numbr d))
        | n == 0 = show 0
        | d == 1 = show n
        | (n > 0 && d < 0) || (n < 0 && d < 0 ) = show (-n) ++ "/" ++ show (-d)
        | otherwise = show n ++ "/" ++ show d
    show (F n f@(F _ _)) = show n ++  "/" ++  show f

mul :: Fraction -> Fraction -> Fraction
mul f f' = mul' (simplify f) (simplify f') where
    mul' (Numbr p) (Numbr q)               = Numbr (p * q)
    mul' (Numbr p) (F p' (Numbr q'))       = F (p * p') (Numbr q')
    mul' (F p' (Numbr q')) (Numbr p)       = F (p * p') (Numbr q')
    mul' (F p (Numbr q)) (F p' (Numbr q')) = reduce $ F (p * p') (Numbr (q * q'))


add :: Fraction -> Fraction -> Fraction
add f f' = add' (simplify f) (simplify f') where
    add' (Numbr p) (Numbr q)               = Numbr (p + q)
    add' (Numbr p) (F p' (Numbr q'))       = F (q' * p + p') (Numbr q')
    add' (F p' (Numbr q')) (Numbr p)       = F (q' * p + p') (Numbr q')
    add' (F p (Numbr q)) (F p' (Numbr q')) = reduce $ F (p * q' + q * p') (Numbr (q * q'))


sub :: Fraction -> Fraction -> Fraction
sub f f' = sub' (simplify f) (simplify f') where
    sub' (Numbr p) (Numbr q)               = Numbr (p - q)
    sub' (Numbr p) (F p' (Numbr q'))       = F (q' * p - p') (Numbr q')
    sub' (F p' (Numbr q')) (Numbr p)       = F (q' * p - p') (Numbr q')
    sub' (F p (Numbr q)) (F p' (Numbr q')) = reduce $ F (p * q' - q * p') (Numbr (q * q'))


divid :: Fraction -> Fraction -> Fraction
divid f f' = divid' (simplify f) (simplify f') where
    divid' (Numbr p) (Numbr q)               = F p (Numbr q)
    divid' (Numbr p) (F p' (Numbr q'))       = F (p * q') (Numbr p')
    divid' (F p' (Numbr q')) (Numbr p)       = F p' (Numbr (q' * p) )
    divid' (F p (Numbr q)) (F p' (Numbr q')) = reduce $ F (p * q') (Numbr (q * p'))

simplify :: Fraction -> Fraction
simplify f = foldr simplify' lastFraction remainingFractions where
    flat = flatten f
    lastFraction = last flat
    remainingFractions = takeWhile (/= lastFraction) flat
    simplify' :: Fraction -> Fraction -> Fraction
    -- these are fractions that are p/q i.e q is not a recurring fraction
    simplify' (Numbr p) (Numbr q)               = F p (Numbr q)
    simplify' (Numbr p) (F p' (Numbr q) )       = F (p * q) (Numbr p')
    simplify' (F p' (Numbr q) ) (Numbr p)       = F (p * p') (Numbr 1)
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
    abs (F n d)   = F  (abs n) (abs d)
    signum (Numbr n) = Numbr (signum n)
    signum (F n d)
        | n == 0 =  Numbr 0
        | n > 0 && d < 0 = Numbr (-1)
        | n < 0 && d > 0 = Numbr (-1)
        | otherwise = Numbr 1
    fromInteger = Numbr

instance Fractional Fraction where
    --{-# MINIMAL fromRational, (recip | (/)) #-}
    fromRational x = F (numerator x) (Numbr (denominator x))
    (/) = divid


-- Take a possibly recursive fraction and reduce it to a list of fractions
flatten :: Fraction -> [Fraction]
flatten f@(F _ (Numbr _) ) = [f]
flatten (Numbr n)          = [Numbr n]
flatten (F n f)            = Numbr n : flatten f



f1, f2, f3, f4 :: Fraction
f1 = 2 + F 2 3
f2 = 6 + F 4 f1
f3 = F 6 f2
f4 = F 1 f3
z  = F 1 (F 1 (F 4 6))

a = 2 + F 1 2
b = 2 + F 1 a
c = 2 + F 1 b
d = 1 + F 1 c
-- rf (-1) = g 0

rf n  t
    | n > t = 0
    | otherwise =  g n + F 1 (rf (n + 1) t ) where
        g 0 = 1
        g _ = 2

rf' n = rf 0 n
    --g' n + F 1 (rf (n - 1))
