module S.Data where

import S.Size
import Data.Hashable

data T = S 
       | O
       | J
       | B -- \ a b c ->  a (b c)
       | D -- \ x -> x x
       | App { _hash :: ! Int, _size :: ! Integer, fun :: ! T, arg :: ! T }
       | Var { _hash :: ! Int, idx :: ! Int }
    deriving ( Eq, Ord )


subterms :: T -> [T]
subterms t = t : case t of
    App {fun=f,arg=a} -> [f,a] >>= subterms
    _ -> []

varfree t = not $ any isVar $ subterms t

isVar (Var {} ) = True; isVar _ = False

fold :: a -> (a -> a -> a) -> T -> a
fold s a t = case t of
    S -> s
    App {fun=x,arg=y} -> a (fold s a x) (fold s a y)

instance Size T where 
    size t = case t of App {} -> _size t ; _ -> 1
    
-- | for backward compatibility
terms = terms_for [s]

terms_for :: [T] -> [[T]]
terms_for base = 
    let terms = [] : base : do
            s <- [2 .. ]
            return $ do
                sl <- [1..s-1] ; let sr = s - sl
                x <- terms !! sl
                y <- terms !! sr
                return $ app x y
    in  terms

normalforms = normalforms_for [s]

normalforms_for :: [T] -> [[T]]
normalforms_for base = error "normalforms_for base"
{-
    let normalforms = []: base : do 
            z <- [2..]
            return $ 
                map (\ n -> unspine [s,n] ) (normalforms !! (z-1))
                ++ do 
                    zl <- [1..z-2] ; let zr = z - 1 - zl
                    x <- normalforms !! zl ; y <- normalforms !! zr
                    return $ unspine [s,x,y]
    in  normalforms
-}

instance Hashable T where
    hashWithSalt s t = hashWithSalt s $ case t of
        S -> 314159
        O -> 141593
        J -> 415931
        B -> 873251
        D -> 310987
        Var {} -> _hash t
        App {} -> _hash t

s :: T
s = S

o :: T
o = O

j :: T
j = J

b :: T ; b = B

d :: T ; d = D

t :: T
t = app s s

a :: T
a = app t s

var :: Int -> T
var i = Var { _hash = hash (271828 :: Int, i), idx = i }

app :: T -> T -> T
app x y = App { fun = x, arg = y, _hash = hash (x, y), _size = size x + size y }

spine :: T -> [T]
spine t = spine_with [] t

spine_with ts t = case t of
    App {fun=f, arg=a} -> spine_with (a:ts) f
    _ -> t : ts

unspine :: [T] -> T
unspine (t : ts) = foldl app t ts

