module L.Reduce where

import L.Type

import qualified S.Type as S
import qualified S.Table as S

import Control.Monad ( forM_, when )
import System.IO 

find_deep_vars = forM_ (concat S.terms) $ \ t -> do
    let (pre,post) = splitAt 200 $ omo $ froms t
    printf (t, length pre, maximum $ do Var i <- pre >>= subterms ; return i )

find_weak_strong = -- forM_ (concat S.terms) $ \ t -> when (S.normalizing t) $ do
    forM_ (concat S.normalforms) $ \ t -> do
        let (pre, post) = splitAt 500 $ imo $ froms t
            s = size $ last pre
        when ( s > 10^6  ) $ do
            printf ( t, length pre )
            forM_ (map size pre) $ \ s -> do hPutStr stdout (show s ++ " ") ; hFlush stdout

printf x = do print x ; hFlush stdout

imo :: L -> [L]
imo t = t : case next_imo t of
    [] -> []
    s : _ -> imo s


next_imo :: L -> [L]
next_imo t = ( case t of
     App {fun=f,arg=a} -> map (\a'-> app f a') (next_imo a)
                       ++ map (\f'-> app f' a) (next_imo f)
     Lam {body=b} -> map lam $ next_imo b
     _ -> [] 
    ) ++ here t

omo :: L -> [L]
omo t = t : case next_omo t of
    [] -> []
    s : _ -> omo s

next_omo :: L -> [L]
next_omo t = here t ++ ( case t of
     App {fun=f,arg=a} -> map (\a'-> app f a') (next_omo a)
                       ++ map (\f'-> app f' a) (next_omo f)
     Lam {body=b} -> map lam $ next_omo b
     _ -> [] 
    ) 

here :: L -> [L]
here t = case t of
    App {fun=App{fun=App{fun=f,arg=x},arg=y},arg=z} | f == the_s ->
         [app (app x z)(app y z)]
    App {fun=Lam{body=b},arg=a} -> [ subst 0 a b ]
    _ -> []

-- | replace variable i with a in b
subst i a b = case b of
    Var j -> if i == j then a else if i < j then var (j-1) else var j
    App {fun=bf,arg=ba} -> app (subst i a bf) (subst i a ba)
    Lam {body=bb} -> lam $ subst (i+1) (up 0 a) bb

-- | increase index of all free variables by one,
up from t = case t of
    Var i -> var $ if i >= from then i + 1 else i
    Lam {body=b} -> lam $ up (from + 1) b
    App {fun=f,arg=a} -> app (up from f) (up from a)
