module S.Data where

import Data.Hashable

data T = S | App { _hash :: ! Int, fun :: ! T, arg :: ! T }
    deriving ( Eq, Ord )

instance Hashable T where
    hashWithSalt s t = hashWithSalt s $ case t of
        S -> 314159
        App {} -> _hash t

s :: T
s = S

t :: T
t = app s s

a :: T
a = app t s

app :: T -> T -> T
app x y = App { fun = x, arg = y, _hash = hash (x, y) }

spine :: T -> [T]
spine t = spine_with [] t

spine_with ts t = case t of
    S -> t : ts
    App {fun=f, arg=a} -> spine_with (a:ts) f

unspine :: [T] -> T
unspine (t : ts) = foldl app t ts

