module FractionParser where
import           Data.List
import           Fractions
import           Text.Megaparsec
import           Text.Megaparsec.String

type Digit  = Integer
type First  = Digit
type Second = [Digit]
type Repeat = [Digit]

data FracStruc = FracStruc { first :: First, repeat :: Repeat} deriving (Show)

dropSpaces :: Parser a -> Parser a
dropSpaces p = space *> p <* space

digitSep :: Char -> Parser Digit
digitSep = dropSpaces . digitSep' where
        digitSep'  :: Char -> Parser Digit
        digitSep'  ch = do
            d <-  some digitChar
            space
            many . char $ ch
            return (read d)

firstP :: Parser Digit
firstP = dropSpaces . digitSep $ ';'

repeatP :: Parser Repeat
repeatP = many . digitSep $ ','

builder = char '[' *>
       (firstP >>= \firstDig -> repeatP >>= \rep -> return $ FracStruc firstDig rep)
       <* char ']'

makeStruct :: String -> Maybe FracStruc
makeStruct txt =
    case runParser builder "" txt of
        Right s -> Just s
        _       -> Nothing

genFa :: Maybe FracStruc -> (Integer -> Fraction)
genfa Nothing = const 1
genFa (Just (FracStruc fs rep)) =
    \n -> if n == 0 then Numbr fs else Numbr (g n) where
        g n = rep !! ix where
            ix = rem (fromIntegral (n - 1)) (length rep)

frac str  = contFrac fa fb  where
    fa = genFa . makeStruct $ str
    fb = const 1


