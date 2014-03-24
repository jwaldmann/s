module L.Eval.Generic where

import qualified S.Type as S

data Algebra v = Algebra { var :: v
                         , lam :: v -> v
                         , app :: v -> v -> v
                         }

size :: Algebra Integer
size = Algebra 
     { var = 1, lam = succ, app = (+) }

depth :: Algebra Integer
depth = Algebra 
      { var = 0, lam = succ, app = \ f a -> succ $ max f a }

max_left_depth :: Algebra (Int, Int)
max_left_depth = Algebra
     { var = (0,0)
     , lam = \ (h,m) -> (0, m)
     , app = \ (fh,fm) (ah,am) -> (succ fh, maximum[succ fh,fm,am])
     }


render :: Algebra String
render = Algebra
    { var = "v"
    , lam = \ b -> "\\v->" ++ b
    , app = \ f a -> "(" ++ f ++ a ++ ")"
    }
    

check1 = eval size $ read "(s t s t s)"

check2 = eval size $ read "(t a (s (s t s t s)))"

check3 = eval depth $ read "(t a (s (s t s t s)))"

check4 = eval size $ read "(a(s(stt(t(t(t(t(t(sts)))))))))"


-- | conj: term k has beta-nf of depth t^3 and size 2^(t^2)
term k = S.app S.a $ S.app S.s 
     $ S.app (read "(stt)")
     $ foldr S.app (read "(sts)") $ replicate k S.t



-- | size of beta-normal form
-- (will not return if there is no nf)
eval :: Algebra v -> S.T -> v
eval alg t = measure alg $ build alg t

build alg t = case t of
    S.S {} -> Fun $ \ x -> Fun $ \ y -> Fun $ \ z -> 
        apply alg (apply alg x z) (apply alg y z)
    S.App {S.fun = f, S.arg = a} -> 
       apply alg (build alg f) (build alg a)

-- | the Val type represents semantics for (head?) normal forms:
data Val v = Fun   (Val v -> Val v) 
             -- ^ a normal form of shape (\ x -> b)
         | Val  v
             -- ^ a normal form of shape (Var .. .. ..)

apply :: Algebra v -> Val v -> Val v -> Val v
apply alg (Fun f) a = f a
apply alg (Val s) a = Val $ app alg s $ measure alg a

measure :: Algebra v -> Val v -> v
measure alg (Val s) = s
measure alg (Fun f) = lam alg $ measure alg $ f $ Val $ var alg
