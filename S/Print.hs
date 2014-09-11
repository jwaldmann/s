{-# language OverloadedStrings #-}

module S.Print where

import S.Data
import Text.PrettyPrint.Leijen
import S.ToDoc

instance ToDoc T where
    toDoc x = case spine x of
        J:[] -> "j"
        S:S:S:[] -> "a"
        S:S:S:rest ->   
            parens $ align $ fillSep $ "a" : map toDoc rest
        S:S:[] -> "t"
        S:S:rest ->   
            parens $ align $ fillSep $ "t" : map toDoc rest
        t:[] -> atom t
        t : rest -> parens $ align $ fillSep $ atom t :  map toDoc rest

atom t = case t of 
    S -> "s" 
    J -> "j" 
    Var{} -> toDoc (idx t) 

instance Show T where show = show . toDoc

