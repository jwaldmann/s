module S.Normal where

import S.Type
import S.Size
import S.Cache
import S.Reduce (here)

import qualified S.Table ( normalizing )

import Control.Concurrent.STM
import Control.Monad (forM_, when)


import qualified Data.Map.Strict as M

import qualified Control.Monad.State.Strict as S
import Control.Monad ( forM )
import System.IO

import Debug.Trace(trace)

-- | compute the normal form by innermost reduction.
-- this will diverge if argument has no normal form.
normalform t = case t of
    App{fun=f,arg=a} -> plapp (normalform f) (normalform a)
    _ -> t

-- | find normal form for application where fun and arg are in nf already
plapp f z = case f of
    App{fun=App{fun=S,arg=x},arg=y} -> 
                 plapp (plapp x z) (plapp y z)
    App{fun=App{fun=App{fun=App{fun=J,arg=a},arg=b},arg=c},arg=d} -> 
        plapp (plapp a b) (plapp (plapp a d) c)
    _ -> app f z

-- | result of at most steps reductions, using cache
normal steps t = S.evalState ( normalize steps t ) 
               $ Cache { m = M.empty, st = steps }


-- | aggressively normalize the given term.
-- return Just normalform, or Nothing if it could not be found
-- with the given recursion depth.
normalize :: Int -> T -> S.State Cache (Maybe T)
normalize steps t = do
    S.modify $ \ c -> c { st = steps }
    cached_fix norm_tricky steps t

cached_fix f _ t = do
    c <- S.get
    if st c < 0 then return Nothing else 
      case M.lookup t $ m c of
        Just res -> return res
        Nothing -> do
            S.modify $ \ c -> c { st = pred $ st c }
            res <- f ( cached_fix f undefined ) t
            S.modify $ \ c -> c { m = M.insert t res $ m c }
            return res


norm_tricky self t = -- ( if size t > 10^6 then trace $ show $ size t else id ) $ 
    if varfree t 
    then return $ if S.Table.normalizing t 
                  then Just $ normalform t else Nothing
    else case t of
      App {fun=f,arg=a} -> do
        nf <- self f
        case nf of
            Nothing -> return Nothing
            Just nf -> do
                 na <- self a
                 case na of
                     Nothing -> return Nothing 
                     Just na -> do
                         let t1 = app nf na 
                         case here t1 of
                             [] -> return $ Just t1
                             t2 : _ -> self t2
      _ -> return $ Just t



norm self t = -- ( if size t > 10^6 then trace $ show $ size t else id ) $ 
  case t of
    App {fun=f,arg=a} -> do
        nf <- self f
        case nf of
            Nothing -> return Nothing
            Just nf -> do
                 na <- self a
                 case na of
                     Nothing -> return Nothing 
                     Just na -> do
                         let t1 = app nf na 
                         case here t1 of
                             [] -> return $ Just t1
                             t2 : _ -> self t2
    _ -> return $ Just t
                          


find_monster = do
    top <- atomically $ newTVar 0
    forM_ (concat $ terms_for [J]) $ \ t -> do   
        let sizes = do
                food <- [ 0 .. 50 ]
                let t' = unspine $ t : ( map var $ take food [ 10 .. ] )
                return $ size $ normalform t'
        best <- atomically $ readTVar top
        when ( maximum sizes > best ) $ do
                let printf x = do print x ; hFlush stdout
                printf (t,sizes)
                atomically $ writeTVar top $ maximum sizes
