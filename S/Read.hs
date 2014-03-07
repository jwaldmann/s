module S.Read where

import S.Data

import Text.ParserCombinators.ReadP

instance Read T where
    readsPrec p = readP_to_S term

term =  do charskip 's' ; return s
    +++ do charskip 't' ; return t
    +++ do charskip 'a' ; return a
    +++ parens ( fmap unspine $ many term )

charskip c = do 
    char c ; skipSpaces

parens p = do 
    charskip '(' ; x <- p ; charskip ')' ; return x
