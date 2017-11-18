module FractionParser where
import           Data.List
import           Fractions
import           Text.Megaparsec
import           Text.Megaparsec.String

type Digit  = Int
type First  = Digit
type Second = [Digit]
type Repeat = [Digit]

data FracStruc = FracStruc { first :: First, repeat :: Repeat} deriving (Show)

dropSpaces :: Parser a -> Parser a
dropSpaces p = do
    space
    v <- p
    space
    return v

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
repeatP = some . digitSep $ ','

builder = char '[' *>
       (firstP >>= \firstDig -> repeatP >>= \rep -> return $ FracStruc firstDig rep)
       <* char ']'

