{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fno-warn-type-defaults #-}
module FractionParser where
import           Control.Applicative
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Parser = Parsec String String
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
            _ <- many . char $ ch
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
