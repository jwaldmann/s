module S.Model where

import S.Type
import S.Context
import S.Reduce (leftmost)

import qualified Data.Map.Strict as M
import Control.Monad ( guard )

build :: Model -> IO Model
build m = do
    -- putStrLn "build" ; print m
    case missing m of
        [] -> return m
        (p,q):_ -> do
            let t = app (base m M.! p) (base m M.! q)
            m' <- classify (p,q) t m
            build m'

classify (p,q) t m = do
    putStrLn "classify" ; print ((p,q),t)
    let handle [] = do
            let i = M.size $ base m
            putStrLn "fresh" ; print i
            return $ m
                   { base = M.insert i t $ base m
                   , trans = M.insert (p,q) (Just i) $ trans m
                   }
        handle ((k,v): kvs) = 
            if equiv (base m) (con m) (dep m) t v
            then do
                 putStrLn "equiv" ; print (k,v)
                 return $ m { trans = M.insert (p,q) (Just k) $ trans m }
            else do
                 handle kvs 
    if not $ null $ drop (dep m) $ leftmost t
       then do
            putStrLn "is non-terminating"
            return $ m { trans = M.insert (p,q) Nothing $ trans m}
       else do
            -- putStrLn "is terminating"
            handle (M.toList $ base m)


missing m = do
    (k2,v2) <- M.toList $ base m
    (k1,v1) <- M.toList $ base m
    guard $ not $ M.member (k1,k2) $ trans m
    return (k1,k2)

equiv base c d t1 t2 = and $ do
    s <- [0..c] 
    --- con <- contexts_over (M.elems base) s
    con <- contexts s
    let n1 = leftmost $ plugin con t1 
        n2 = leftmost $ plugin con t2 
    return $ null (drop d n1) == null (drop d n2)

contexts_over base s = if s <= 0 then [ This ] else do
    t <- base
    c <- contexts_over base (s-1)
    [ L t c, R c t ]

data Model = Model { base :: M.Map Int T 
                 , trans :: M.Map (Int,Int) (Maybe Int)
           , con :: Int, dep :: Int
             } deriving Show

model0 c d = 
   Model { base = M.singleton 0 s , trans = M.empty, con=c,dep=d }

