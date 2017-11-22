{-# OPTIONS_GHC -Wall #-}
module FractionParser where
import           Control.Applicative
import           Control.Monad
import           Data.List
import           Fractions
import           Text.Megaparsec
import           Text.Megaparsec.String
type Digit  = Integer
type First  = Digit
type Repeat = [Digit]


-- [a1;a2,a3,a4...] => first = a1, repeat = [a2,a3,a4]
data FracStruc = FracStruc { first :: First, repeat :: Repeat} deriving (Show)

-- right and left bird :). Put the parser in the middle
dropSpaces :: Parser a -> Parser a
dropSpaces p = space *> p <* space

-- A number followed by a separator char - for Fractions this will be ; or ,
digitSep :: Char -> Parser Digit
digitSep = dropSpaces . digitSep1 where
        digitSep1 :: Char -> Parser Digit
        digitSep1  ch = do
            d <- some digitChar
            space
            many . char $ ch
            return (read d)

-- Parse out the a1 in [a1;a2,a3...]
firstP :: Parser Digit
firstP = dropSpaces . digitSep $ ';'

-- Parse out the a2, a3... in [a1;a2, a3...]
repeatP :: Parser Repeat
repeatP = many . digitSep $ ','

-- Parse a FracStruc from "[a1;a2, a3...]"
fracStrucP :: Parser FracStruc
fracStrucP = char '[' *>
       (firstP >>= \firstDig -> repeatP >>= \rep -> return $ FracStruc firstDig rep)
                   <* char ']'

-- Runs fracStrucP to maybe make a FracStruc
makeStruct :: String -> Maybe FracStruc
makeStruct txt =
    case runParser fracStrucP "" txt of
        Right s -> Just s
        _       -> Nothing

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
