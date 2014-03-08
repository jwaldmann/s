module S.Model where

import S.Type
import S.Context
import S.Reduce (leftmost)
import S.Normal (normalize)

import Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import Control.Concurrent.STM
import Control.Monad ( guard )
import Data.Maybe (isJust)
import System.IO

build :: Model -> IO Model
build m = do
    -- putStrLn "build" ; print m
    case missing m of
        [] -> return m
        (p,q):_ -> do
            let t = app (base m M.! p) (base m M.! q)
            m' <- normal_classify (p,q) t m
            build m'

norma m t = do
    s0 <- atomically $ readTVar $ state m
    let (n,s1) = runState (normalize (dep m) t) s0
    atomically $ writeTVar (state m) s1
    -- putStrLn $ unwords [ "norm", show (M.size s1), show t, show n ]
    return n

normal_classify (p,q) t m = do
    putStrLn "normal_classify" ; printf ((p,q),t)
    n <- norma m t
    classify (p,q) n  m

classify (p,q) Nothing m = do
    putStrLn "is non-terminating"
    return $ m { trans = M.insert (p,q) Nothing $ trans m}

classify (p,q) (Just t) m = do
    putStrLn "classify" ; printf ((p,q),t)
    let handle [] = do
            let i = M.size $ base m
            putStrLn "fresh" ; printf i
            return $ m
                   { base = M.insert i t $ base m
                   , trans = M.insert (p,q) (Just i) $ trans m
                   }
        handle ((k,v): kvs) = do
            e <- equiv m t v
            if e
            then do
                 putStrLn "equiv" ; printf (k,v)
                 return $ m { trans = M.insert (p,q) (Just k) $ trans m }
            else do
                 handle kvs 
    handle (M.toList $ base m) 


missing m = do
    (k2,v2) <- M.toList $ base m
    (k1,v1) <- M.toList $ base m
    guard $ not $ M.member (k1,k2) $ trans m
    return (k1,k2)

forall [] f = return True
forall (x:xs) f = do
    y <- f x
    case y of
        False -> return False
        True  -> forall xs f

equiv m t1 t2 = 
    forall ([0 .. con m] >>= contexts) $ \ con -> do
        n1 <- norma m $ plugin con t1 
        n2 <- norma m $ plugin con t2 
        return $ isJust n1 == isJust n2



data Model = Model { base :: ! ( M.Map Int T )
                 , trans :: ! ( M.Map (Int,Int) (Maybe Int) )
                 , state :: TVar ( M.Map T (Maybe T) )
           , con :: ! Int, dep :: ! Int
             } 

model0 c d = do
    st <- atomically $ newTVar M.empty
    return $ Model 
         { base = M.singleton 0 s , trans = M.empty
         , state = st
         , con=c,dep=d 
         }

printf x = do print x ; hFlush stdout
