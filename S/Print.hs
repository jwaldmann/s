{-# language OverloadedStrings #-}

module S.Print where

import S.Data
import Text.PrettyPrint.Leijen
import S.ToDoc

instance ToDoc T where
    toDoc t = case t of
        S -> "s"
        _ -> parens $ align $ fillSep $ map toDoc $ spine t

instance Show T where show = show . toDoc

