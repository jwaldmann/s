{-# language StandaloneDeriving #-}
{-# language DisambiguateRecordFields #-}

module L.Type 

( module L.Data
, froms
)

where

import L.Data
import L.Print
import qualified S.Type as S


froms :: S.T -> L
froms t = case t of
    S.S -> the_s
    S.B -> the_b
    S.J -> the_j
    S.App { fun=f,arg=a } -> app (froms f) (froms a)
