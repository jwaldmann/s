{-# language OverloadedStrings #-}

module S.Print where

import S.Data
import Text.PrettyPrint.Leijen
import Data.String

instance IsString Doc where
    fromString = text

instance Pretty T where
    pretty t = case t of
        S -> "S"
        _ -> parens $ fillSep $ map pretty $ spine t

instance Show T where show = show . pretty
