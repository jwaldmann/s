module S.Normal where

import S.Type
import S.Reduce (here)

import qualified Data.Map.Strict as M

import qualified Control.Monad.State.Strict as S

-- | compute the normal form by innermost reduction.
-- this will diverge if argument has no normal form.
normalform t = case t of
    App{fun=f,arg=a} -> plapp (normalform f) (normalform a)
    _ -> t

-- | find normal form for application where fun and arg are in nf already
plapp f z = case f of
    App{fun=App{fun=S,arg=x},arg=y} -> 
                 plapp (plapp x z) (plapp y z)
    App{fun=S,arg=x} -> app f z
    S -> app f z

-- | result of at most steps reductions, using cache
normal steps t = S.evalState ( normalize steps t ) M.empty

-- | aggressively normalize the given term.
-- return Just normalform, or Nothing if it could not be found
-- with the given recursion depth.
normalize :: Int -> T -> S.State (M.Map T (Maybe T)) (Maybe T)
normalize steps t = cached_fix norm steps t

cached_fix f s t = do
    m <- S.get
    case M.lookup t m of
        Just res -> return res
        Nothing -> do
            res <- if s > 0 then f ( cached_fix f (s-1) ) t
                            else return Nothing
            S.modify $ \ m -> M.insert t res m
            return res

norm self t = case t of
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
                          
