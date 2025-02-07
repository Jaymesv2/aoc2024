module Util.Parsers (decimal, number) where
import Data.Text (Text)
import Text.Parsec
import Data.Char

decimal :: Num n => Parsec Text () n
decimal = number 10 digit

number :: ( Stream s m t, Num n ) => n -> ParsecT s u m Char -> ParsecT s u m n
number base baseDigit
    = do{ digits <- many1 baseDigit
        ; let n = foldl (\x d -> base*x + fromInteger (toInteger (digitToInt d))) 0 digits
        ; seq n (pure n)
        }

