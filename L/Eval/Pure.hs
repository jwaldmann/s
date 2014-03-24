module L.Eval.Pure where

import qualified S.Type as S

-- | example due to Barendregt (NF size approx 10^6)
check1 = eval $ read "(ststs)"

-- | huge and quick (guess the size of the NF before evaluating!)
check2 = eval $ read "(t a (s (s t s t s)))"

-- | size of beta-normal form
-- (will not return if there is no nf)
eval :: S.T -> Integer
eval t = measure $ build t

build t = case t of
    S.S {} -> s
    S.App {S.fun = f, S.arg = a} -> app (build f) (build a)

data Val = Fun { unFun :: ! (Val -> Val) }
         | Val { unVal :: ! Integer } 

s = Fun $ \ x -> Fun $ \ y -> Fun $ \ z -> 
    app (app x z) (app y z)


app (Fun f) a = f a
app (Val s) a = Val $ s + measure a
    -- the application operator is not counted

measure (Val s) = s
measure (Fun f) = succ -- this counts 1 for the lambda
    $ measure $ f (Val 1) -- this counts one for the variable
