module S.ModelIO where

import S.Type
import S.Context
import S.ToDoc

import qualified S.Normal
import qualified S.Table as ST

import qualified L.Eval.Pure ( eval_musec )

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Control.Monad ( guard, when )
import Data.Maybe (isJust, isNothing)
import System.IO
import Control.Concurrent.STM

type Normalize a = Int -> T -> IO (Maybe a)


build_beta_full = build $ L.Eval.Pure.eval_musec


build :: Show a => Normalize a -> Model a -> IO (Model a)
build normalize m = do
    -- putStrLn "build" ; print m
    case missing m of
        [] -> return m
        (p,q):_ -> do
            let t = app (base m M.! p) (base m M.! q)
            m' <- classify normalize (p,q) t m
            build normalize m'

norma lize m t = 
    if not $ ST.normalizing t then return Nothing
    else do
        let n = S.Normal.normalform t      
        c <- atomically $ readTVar $ cache m
        case M.lookup n c of
            Just res -> return res
            Nothing ->  do
                res <- lize (dep m) n
                atomically $ writeTVar ( cache m ) 
                       $ M.insert n res c
                return res

classify lize (p,q) t mo = do
    putStrLn "classify" ; printf ((p,q),t)
    n <- norma lize mo t
    if isNothing n then do
        let i = M.size $ base mo
        putStrLn  "non-terminating"
        return $ mo
               { trans = M.insert (p,q) Nothing $ trans mo
               }
    else do
      -- printf n
      let handle [] = do
            let i = M.size $ base mo
            putStrLn "fresh" ; printf i
            return $ mo
                   { base = M.insert i t $ base mo
                   , trans = M.insert (p,q) (Just i) $ trans mo
                   , accept = if isJust n then S.insert i $ accept mo else accept mo
                   }
          handle ((k,v): kvs) = do
            -- putStrLn $ "handle" ++ show (k,v)
            e <- equiv lize mo t v
            if e
            then do
                 putStrLn "equiv" ; printf (k,v)
                 return $ mo { trans = M.insert (p,q) (Just k) $ trans mo }
            else do
                 handle kvs 
      handle (M.toList $ base mo ) 


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

equiv lize m t1 t2 = 
    forall ([0 .. con m] >>= contexts) $ \ con -> do
        -- print (t1, t2, con)

        n1 <- norma lize m $ plugin con t1 
        n2 <- norma lize m $ plugin con t2 

        when (False && ( isJust n1 /= isJust n2 ) ) $ do
            print con
            print ( plugin con t1,plugin con t2)
            print (n1, n2)

        return $ isJust n1 == isJust n2



data Model a = Model { base :: ! ( M.Map Int T )
                 , trans :: ! ( M.Map (Int,Int) (Maybe Int) )
                 , accept :: ! (S.Set Int)
           , cache :: TVar ( M.Map T (Maybe a))
           , con :: ! Int, dep :: ! Int
             } 

model0 c d = do
    ca <- atomically $ newTVar $ M.empty
    return $ Model 
         { base = M.singleton 0 s 
         , trans = M.empty, accept = S.empty
         , cache = ca
         , con=c,dep=d 
         }

printf x = do print x ; hFlush stdout
