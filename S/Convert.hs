module S.Convert where

import S.Type

-- | pair of equivalent ground terms
type Axiom = (T,T)

equivalent :: [(T,T)] -> T -> [T]
equivalent axioms t = undefined 

