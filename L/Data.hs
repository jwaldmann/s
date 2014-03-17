module L.Data 

( module L.Data
, size
)

where

import S.Size

import Data.Hashable

data L = Var Int 
       | App { _hash :: !Int, _size :: !Int, fun :: !L , arg :: !L }
       | Lam { _hash :: !Int, _size :: !Int , body :: L }
    deriving ( Eq, Ord )

var i = Var i
app f a = App { _hash = hash (f,a) , _size = size f + size a, fun = f, arg = a }
lam b = Lam { _hash = hash (42 :: Int,b) , _size = 1 + size b , body = b }

the_s = lam $ lam $ lam $ app (app (var 2) (var 0)) (app (var 1) (var 0))

instance Hashable L where
    hashWithSalt s t = hashWithSalt s $ case t of
        Var i -> hashWithSalt 314159 i
        _ -> _hash t

instance Size L where
    size t = case t of
        Var i -> 1
        _ -> _size t

spine :: L -> [L]
spine t = spine_with [] t

spine_with ts t = case t of
    App {fun=f, arg=a} -> spine_with (a:ts) f
    _ -> t : ts

unspine :: [L] -> L
unspine (t : ts) = foldl app t ts
