module L.Eval.Monadic where

import qualified S.Type as S

import qualified Control.Monad.Writer as W
import Control.Monad ( foldM )


-- | size of beta-normal form (if reached by s innermost steps)
eval :: Int -> S.T -> Maybe (Int,Integer)
eval s t = 
    let (a,w) = W.runWriter ( build t >>= measure )
    in  if null $ drop s w then Just (length w, a) else Nothing

build t = case t of
    S.S {} -> return s
    S.App {} -> do 
       f <- build $ S.fun t ; a <- build $ S.arg t ; app f a

data Val = Fun { unFun :: ! (Val -> W.Writer [()] Val) }
         | Val { unVal :: ! Integer } 

s = Fun $ \ x -> return $ Fun $ \ y -> return $ Fun $ \ z -> 
    -- app (app x z) (app y z)
    app x z >>= \ xz -> app y z >>= \ yz -> app xz yz


check = W.runWriter $ do
    t <- app s s ; a <- app t s
    mo <- foldM app s [ t,s,t,s ]
    measure mo

-- app :: Val -> Val -> S.State Int Val
app (Fun f) a = do
    W.tell [()] ; f a
app (Val s) a = do
    m <- measure a
    return $ Val $ s + m -- the application operator is not counted

measure (Val s) = return s
measure (Fun f) = do
    t <- f (Val 1) -- this counts one for the variable
    m <- measure t
    return $ succ m -- this counts 1 for the lambda
