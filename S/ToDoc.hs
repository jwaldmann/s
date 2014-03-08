{-# language OverloadedStrings #-}

module S.ToDoc where

import Text.PrettyPrint.Leijen 
import Data.String

import qualified Data.Map as M

-- | the printout should be readable
-- by the instance we get from "deriving Read",
-- so "show (toDoc x)" can differ only in whitespace
-- from "show x" (for the "deriving Show" instance).
-- The "Pretty" instances from Text.PrettyPrint.Leijen are wrong,
-- e.g., pretty 'a' ==> a (no quotes), pretty Nothing => empty

class ToDoc a where toDoc :: a -> Doc

instance ToDoc Int where toDoc = text . show
instance ToDoc Bool where toDoc = text . show

instance ToDoc a => ToDoc (Maybe a) where
    toDoc m = case m of
        Nothing -> "Nothing"
        Just x -> "Just" <+> align ( toDoc x )

instance (ToDoc a, ToDoc b) => ToDoc (a,b) where
    toDoc (x,y) = parens $ align $ fillSep $ punctuate "," [ toDoc x, toDoc y ]

instance (ToDoc a, ToDoc b, ToDoc c) => ToDoc (a,b,c) where
    toDoc (x,y,z) = parens $ align $ fillSep $ punctuate "," [ toDoc x, toDoc y, toDoc z ]

instance ToDoc a => ToDoc [a] where
    toDoc xs = brackets $ align $ fillSep $ punctuate "," $ map toDoc xs

instance ( ToDoc k, ToDoc v ) => ToDoc (M.Map k v) where
    toDoc m = "M.fromList" <+> align (toDoc (M.toList m) )

instance IsString Doc where fromString = text
