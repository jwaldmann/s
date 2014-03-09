module S.Head where

import S.Type
import S.Reduce (here)

import qualified Data.Map.Strict as M

import qualified Control.Monad.State.Strict as S

-- | member of the set Q Q Q,
-- consequently, t has infinite head reduction
isq3 t = case t of
    App {fun=xy, arg=z} | isq z -> case xy of
        App {fun=x, arg=y} | isq x && isq y -> True
        _ -> False 
    _ -> False

-- | member of the set Q.
--  where P = S/S, Q = M - P,
--  alternatively,  t  has some subterm with left depht > 1
isq t = let S: xs = spine t in case xs of
    [] -> False
    [x] -> isq x
    _ -> True


normal steps t = S.evalState ( normalize steps t ) M.empty

-- | aggressively head-normalize the given term.
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

norm self t | isq3 t = return Nothing
norm self t = case t of
    S -> return $ Just s
    App {fun=f,arg=a} -> do
        nf <- self f
        case nf of
            Nothing -> return Nothing
            Just nf -> do
                let t1 = app nf a 
                case here t1 of
                             [] -> return $ Just t1
                             t2 : _ -> self t2
                          
