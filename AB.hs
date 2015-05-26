{-

(4 3 (2 1 0))

<-  B (4 3) (2 1) 0
<-  B (B (4 3)) 2 1 0
<-  B (B B 4 3) 2 1 0
<-  B B (B B 4) 3 2 1 0
<-  B (B B) (B B) 4 3 2 1 0

-}

module AB where

import S.Type 

import Debug.Trace

abstract :: T -> T
abstract t | varfree t = t
abstract t = trace (show t) $ case t of
  App { fun = x, arg = App { fun = y, arg = z }} ->
    abstract $ unspine [ B, x, y, z ]
  App { fun = x, arg = Var { }} ->
    app (abstract x) (arg t)
    
