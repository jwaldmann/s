module S.Read where

import S.Data

import Text.ParserCombinators.ReadP

instance Read T where
    readsPrec p = readP_to_S term

term = do charskip 'S' ; return s
   +++ parens ( fmap unspine $ many term )

charskip c = do 
    char c ; skipSpaces

parens p = do 
    charskip '(' ; x <- p ; charskip ')' ; return x
