module S.Model where

import S.Type
import qualified S.Cache as C
import S.Context
import S.Reduce (leftmost)
import qualified S.Reduce 
import S.ToDoc

import qualified S.Normal
import qualified S.Head
import qualified L.Eval



import Control.Monad.State.Strict ( runState, State )
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Concurrent.STM
import Control.Monad ( guard, when )
import Data.Maybe (isJust)
import System.IO

type Normalize = Int -> T -> State C.Cache (Maybe T)

write_full_black_table = do
    -- m1 <- model0 6 66  -- this works
    m1 <- model1 6 66  
    mo  <- build S.Normal.normalize m1
    print $ toDoc $ base mo
    print $ toDoc $ trans mo
    print $ toDoc $ accept mo

build_full = build S.Normal.normalize
build_head = build S.Head.normalize


build :: Normalize -> Model -> IO Model
build normalize m = do
    -- putStrLn "build" ; print m
    case missing m of
        [] -> return m
        (p,q):_ -> do
            let t = app (base m M.! p) (base m M.! q)
            -- m' <- normal_classify normalize (p,q) t m
            m' <- classify normalize (p,q) (Just t) m
            build normalize m'

norma lize m t = do
    s0 <- atomically $ readTVar $ state m
    let (n,s1) = runState (lize (dep m) t) 
               $ C.Cache { C.m = s0, C.st = 0 }
    atomically $ writeTVar (state m) $ C.m s1
    -- putStrLn $ unwords [ "norm", show (M.size s1), show t, show n ]
    return n

normal_classify lize (p,q) t mo = do
    putStrLn "normal_classify" ; printf ((p,q),t)
    n <- norma lize mo t
    classify lize (p,q) n mo

mkcache mo = do
    s <- atomically $ readTVar $ state mo
    return $ C.Cache { C.m = s, C.st = 0 }

classify lize (p,q) Nothing mo = do
    putStrLn "is non-terminating"
    return $ mo { trans = M.insert (p,q) Nothing $ trans mo }

classify lize (p,q) (Just t) mo = do
    putStrLn "classify" ; printf ((p,q),t)
    n <- norma lize mo t
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
    (k1,v1) <- reverse $ M.toList $ base m
    (k2,v2) <- M.toList $ base m
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

        when ( False && ( isJust n1 /= isJust n2 ) )
             $ print (con, plugin con t1,plugin con t2)

        return $ isJust n1 == isJust n2



data Model = Model { base :: ! ( M.Map Int T )
                 , trans :: ! ( M.Map (Int,Int) (Maybe Int) )
                 , accept :: ! (S.Set Int)
                 , state :: TVar ( M.Map T (Maybe T))
           , con :: ! Int, dep :: ! Int
             } 

model0 c d = do
    st <- atomically $ newTVar M.empty
    return $ Model 
         { base = M.singleton 0 s , trans = M.empty, accept = S.empty
         , state = st
         , con=c,dep=d 
         }

-- | basis is { S, black-hole }
model1 c d = do
    st <- atomically $ newTVar M.empty
    return $ Model 
         { base = M.fromList [( 0, s), (1, var 0) ] 
         , trans = M.empty, accept = S.empty
         , state = st
         , con=c,dep=d 
         }

printf x = do print x ; hFlush stdout
