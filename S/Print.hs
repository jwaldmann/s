{-# language OverloadedStrings #-}

module S.Print where

import S.Data
import Text.PrettyPrint.Leijen
import S.ToDoc

instance ToDoc T where
    toDoc x = case spine x of
        S:S:S:[] -> "a"
        S:S:S:rest ->   
            parens $ align $ fillSep $ "a" : map toDoc rest
        S:S:[] -> "t"
        S:S:rest ->   
            parens $ align $ fillSep $ "t" : map toDoc rest
        S:[] -> "s"
        S:rest ->   
            parens $ align $ fillSep $ "s" : map toDoc rest

instance Show T where show = show . toDoc

