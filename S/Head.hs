module S.Head where

import S.Type
import S.Cache
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

-- | actual head reduction, no cache, no shortcuts
plain :: T -> [ T ]
plain t = 
    let f xs = unspine xs : case xs of
            S : x : y : z : rest -> f $ spine x ++ z : app y z : rest
            _ -> []
    in  f $ spine t

normalform = last . plain

normal steps t = S.evalState ( normalize steps t ) 
               $ Cache { m = M.empty, st = steps }

-- | aggressively head-normalize the given term.
-- return Just normalform, or Nothing if it could not be found
-- with the given recursion depth.
normalize :: Int -> T -> S.State Cache (Maybe T)
normalize steps t = do
    S.modify $ \ c -> c { st = steps }
    cached_fix norm steps t

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
                          
