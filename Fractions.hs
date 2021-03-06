{-# LANGUAGE OverloadedStrings #-}
-- {-# OPTIONS -Wall -fno-warn-type-defaults #-}
module Fractions where
import           Data.List
import           Data.Ratio
import           FractionParser
type Numerator   = Integer
type Denominator = Integer
{-
So a fraction can be a simple
3 or 3/4 or 3 / (4 / (...)) - a recursive, continued, fraction:
-}
data Fraction  = Numbr Numerator | F Numerator Fraction
type CFList = [Integer]
type Convergents = [Fraction]

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

-- =========================================================

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


-- Takes n/d and returns the CF in list form. eg.
--    41/13 = [3, 6, 2]
--    124/37 = [3, 2, 1, 5, 2]
--    5/12 = [0, 2, 2, 2]
toCFList :: Fraction -> CFList
toCFList (F n (Numbr d))
    | d' == 0 = [a]
    | otherwise =  a : toCFList (F d (Numbr d')) where
       (a, d') = divMod n d


-- Reversing a CF list will give a fraction with the same numerator
reverseCFList :: CFList -> CFList
reverseCFList (0:xs) = reverse xs
reverseCFList xs     = reverse xs

toFracStruc :: CFList -> FracStruc
toFracStruc []     = FracStruc 0 []
toFracStruc (x:xs) = FracStruc x xs

convergentsFromCFList :: CFList -> Convergents
convergentsFromCFList cs = [frac (toInteger d') fracStruc | d' <- [1..]] where
    fracStruc = toFracStruc cs
    len (FracStruc _ r) = 1 + length r

convergentDeltas :: Convergents -> [(Fraction, Float)]
convergentDeltas  cs = map (\ fr -> (fr, abs f - evalFrac fr)) cs where
    f = evalFrac (last cs)

cfListFloat :: Float -> CFList
cfListFloat x = x' : cfListFloat (1 / (x - fromIntegral x' )) where x' = truncate x

isSquare :: Integral a => a -> Bool
isSquare n = sq * sq == n where sq = floor $ sqrt (fromIntegral n :: Double)
-- All CF lists for sqrt are infinite and periodic. Period repeats when
-- current term is twice the first one.
cfListSqrtNFinite :: Integer -> CFList
cfListSqrtNFinite n
 | isSquare n = [floor $ sqrt (fromIntegral n :: Double)]
 | otherwise = takeWhile (/= a02) (cfListSqrtN' n)  ++ [a02] where
    a0 = truncate . sqrt . fromIntegral $ n
    a02 = a0 * 2
    cfListSqrtN' n = [ ai | (_, _, ai) <- terms ] where
      m0 = 0
      d0 = 1
      a0 = truncate . sqrt . fromIntegral $ n
      terms = iterate nextTerm (m0, d0, a0)
      nextTerm (mi, di, ai) = (mj, dj, aj) where
            mj = di * ai - mi
            dj = (n - mj * mj) `div` di
            aj = (a0 + mj) `div` dj

cfListSqrtN :: Integer -> CFList
cfListSqrtN n
 | isSquare n = [floor $ sqrt (fromIntegral n :: Double)]
 | otherwise = cfListSqrtN' n ++ [a02] where
    a0 = truncate . sqrt . fromIntegral $ n
    a02 = a0 * 2
    cfListSqrtN' n = [ ai | (_, _, ai) <- terms ] where
      m0 = 0
      d0 = 1
      a0 = truncate . sqrt . fromIntegral $ n
      terms = iterate nextTerm (m0, d0, a0)
      nextTerm (mi, di, ai) = (mj, dj, aj) where
            mj = di * ai - mi
            dj = (n - mj * mj) `div` di
            aj = (a0 + mj) `div` dj

-- The convergents of a continued fraction expansion of x give the best rational
-- approximations to x. Specifically, the only way a fraction can approximate x
-- better than a convergent is if the fraction has a bigger denominator than the convergent.
convergents :: CFList -> Convergents
convergents [x] = [F x (Numbr 1)]
convergents (a0 : a1 : as) = map (\(n, d) -> F n (Numbr d) ) terms
    where
      p0 = a0
      q0 = 1
      p1 = a1 * a0 + 1
      q1 = a1
      terms = (p0, q0) : (p1, q1) : zipWith3 nextConv terms (tail terms) as
      nextConv (pi, qi) (pj, qj) ak = (pk, qk)
          where
            pk = ak * pj + pi
            qk = ak * qj + qi

convergentsFromFrac :: Fraction -> Convergents
convergentsFromFrac = convergents . toCFList

-- Pells eqn. x^2 - dy^2 = 1
notPellSolution d x y = x*x - d*y*y /= 1

-- First or fundamental solution.
solvePell :: Integer -> (Integer, Integer)
solvePell d = (x, y) where
    F x (Numbr y) = head . dropWhile (\(F p (Numbr q)) -> notPellSolution d p q) . convergents . cfListSqrtN $ d

solvePellAll :: Integer -> [(Integer, Integer)]
solvePellAll n  = [ (x', y') | (x', y') <- terms] where
    (x1, y1) = solvePell n
    terms = iterate nextTerm (x1, y1)
    nextTerm  (x, y) = (x1*x + n*y1*y, x1*y + y1*x)

countDigits :: Integer -> Integer
countDigits n = foldr (\_ a -> a + 1) 0  (show n)

nthSolutionSize :: Int -> Integer -> (Integer, Integer)
nthSolutionSize nth n = (countDigits x, countDigits y) where
    (x, y) = last . take nth . solvePellAll $ n

-- The fa function in contFrac fa fb is fully defined by the values in a FracStruc
genFa ::  FracStruc -> (Integer -> Fraction)
genFa (FracStruc fs rep) =
    \n -> if n == 0 then Numbr fs else Numbr (g n) where
        g n = rep !! ix where
            ix = rem (fromIntegral (n - 1)) (length rep)

-- From the supplied list format and the given depth perhaps create a Fraction
fracFromList :: Integer ->  String -> Maybe Fraction
fracFromList depth s =
    case makeStruct s of
        Nothing    -> Nothing
        Just struc -> Just $ frac depth struc

-- Create a fraction to the supplied depth using the given FracStruc
-- fb assumed fixed at 1
frac :: Integer -> FracStruc -> Fraction
frac depth struc@(FracStruc fs rep)
    | null rep = Numbr fs
    | otherwise = contFrac fa fb  depth where
        fa = genFa struc
        fb = const 1


evalFracM :: Integer -> String -> Maybe Float
evalFracM depth s = fracFromList depth s >>= Just . evalFrac
-- A function for the 'a's for a 'fixed' FracStruc 1 [2]
fa12 :: Integer -> Fraction
fa12 = genFa (FracStruc 1 [2])

-- The nunber and the depth
root :: Integer -> Integer -> Fraction
root n = contFrac fa fb where
    fa   = fa12
    fb _ = n - 1

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
