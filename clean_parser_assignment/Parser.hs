module Parser(module CoreParser, T, digit, digitVal, chars, letter, err,
              lit, number, iter, accept, require, token,
              spaces, word, (-#), (#-)) where
import Prelude hiding (return, fail)
import Data.Char
import CoreParser
infixl 7 -#, #-

type T a = Parser a

err :: String -> Parser a
err message cs = error (message++" near "++cs++"\n")

iter :: Parser a -> Parser [a]
iter m = m # iter m >-> cons ! return []

cons(a, b) = a:b

(-#) :: Parser a -> Parser b -> Parser b -- Accepts b and ignores a
m -# n = (m # n) >-> snd

(#-) :: Parser a -> Parser b -> Parser a -- Accepts a and ignores b
m #- n = (m # n) >-> fst

spaces :: Parser String -- Accepts any number of spaces
spaces = iter (char ? isSpace)

token :: Parser a -> Parser a
token m = m #- spaces

letter :: Parser Char -- Accepts any letter
letter = char ? isAlpha

word :: Parser String
word = token (letter # iter letter >-> cons)

chars :: Int -> Parser String -- Accepts a string of length n
chars n = token (iter (char ? isAlpha) >-> take n)

accept :: String -> Parser String
accept w = token (chars (length w)) ? (==w)

require :: String -> Parser String -- accepts w and reports the error
require w = accept w ! err ("Expected: " ++ w)

lit :: Char -> Parser Char
lit c = token char ? (==c)

digit :: Parser Char
digit = char ? isDigit

digitVal :: Parser Integer
digitVal = digit >-> digitToInt >-> fromIntegral

number' :: Integer -> Parser Integer
number' n = digitVal #> (\ d -> number' (10*n+d))
          ! return n
number :: Parser Integer
number = token (digitVal #> number')

