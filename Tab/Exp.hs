-- | find a decision tree representation 
-- for M.Map (Int,Int) Int
-- by evolutionary optimisation

{-# language OverloadedStrings #-}
   
module Tab.Exp where

import qualified Data.Map.Strict as M

import Text.PrettyPrint.Leijen
import Data.String
       
data Exp = Const Int
         | IfEq Pos Int Exp Exp

size :: Exp -> Int
size x = case x of
    IfEq _ _ l r -> 1 + size l + size r
    _ -> 1
     
data Pos = X | Y
    deriving Enum
    
instance Show Exp where show = show . pretty
instance Show Pos where show = show . pretty

     
value :: Exp -> (Int,Int) -> Int
value e (x,y) = case e of
    Const i -> i
    IfEq p i l r ->
        value
        (if (case p of X -> x ; Y -> y) == i then l else r)
        (x,y)

instance Pretty Exp where
    pretty x = case x of
        Const i -> pretty i
        IfEq p i l r -> vcat
            [ "if" <+> pretty p <+> "==" <+> pretty i
            , "then" <+> align (pretty l)
            , "else" <+> align (pretty r)
            ]
    
instance Pretty Pos where
    pretty p = case p of
        X -> "x"
        Y -> "y"
    
instance IsString Doc where
    fromString = text
