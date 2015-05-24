{-# language OverloadedStrings #-}

module L.Print where

import L.Data
import Text.PrettyPrint.Leijen
import S.ToDoc

instance ToDoc L where
    toDoc x = case spine x of
        this:[] -> single this
        this:rest ->   
            parens $ align $ fillSep 
            $ single this 
            -- : brackets (toDoc (length rest)) 
            : map toDoc rest

single t = case t of
    _ | t == the_s -> "s"
    Var i -> toDoc i
    Lam { body = b } -> align ( "\\." <> toDoc b )

instance Show L where show = show . toDoc

