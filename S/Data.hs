module S.Data where

import S.Size
import Data.Hashable

data T = S | App { _hash :: ! Int, _size :: ! Int, fun :: ! T, arg :: ! T }
    deriving ( Eq, Ord )

fold :: a -> (a -> a -> a) -> T -> a
fold s a t = case t of
    S -> s
    App {fun=x,arg=y} -> a (fold s a x) (fold s a y)

instance Size T where 
    -- size =  fold 1 (+)
    size t = case t of S -> 1 ; App {} -> _size t
    
terms :: [[T]]
terms = [] : [s] : do
    s <- [2 .. ]
    return $ do
        sl <- [1..s-1] ; let sr = s - sl
        x <- terms !! sl
        y <- terms !! sr
        return $ app x y

normalforms :: [[T]]
normalforms = []: [s] : do 
    z <- [2..]
    return $ 
         map (\ n -> unspine [s,n] ) (normalforms !! (z-1))
      ++ do zl <- [1..z-2] ; let zr = z - 1 - zl
            x <- normalforms !! zl ; y <- normalforms !! zr
            return $ unspine [s,x,y]


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
app x y = App { fun = x, arg = y, _hash = hash (x, y), _size = size x + size y }

spine :: T -> [T]
spine t = spine_with [] t

spine_with ts t = case t of
    S -> t : ts
    App {fun=f, arg=a} -> spine_with (a:ts) f

unspine :: [T] -> T
unspine (t : ts) = foldl app t ts

