module L.Read where

import L.Data
import Data.Char (isDigit)
import Text.ParserCombinators.ReadP

instance Read L where
    readsPrec p = readP_to_S term

term =  do i <- natural ; return $ var i 
    +++ parens ( fmap unspine $ many1 term )

charskip c = do 
    char c ; skipSpaces

natural = do
    ds <- many1 (satisfy isDigit) ; skipSpaces
    return $ foldl ( \ a d -> 10 * a + fromEnum d - fromEnum '0' ) 0 ds

parens p = do 
    charskip '(' ; x <- p ; charskip ')' ; return x


