import S.Type
import S.Table

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Function (on)
import Data.Maybe

-- | print all contexts 
-- that map  S to  not-normalizing (state 38), 
-- and  p = not-S  to  normalizing (state /= 38)
-- it turns out that p must be 1 or 8.
-- both states accept only on term:  1 -> T = SS, 8 -> (S A) = (S (SSS))
main =  mapM_ print 
     $ filter ( \ m-> fst (to m) == top && snd (to m) /= top) 
     $ pairs_from fromzero 


type Context = [ Either Int Int ]

data Move = Move 
          { from :: (Int,Int)
          , to :: (Int,Int)
          , by :: Context
          }
    deriving Show

essence m = (from m, to m)

instance Eq Move where (==) = (==) `on` essence
instance Ord Move where compare = compare `on` essence

hull :: Ord a => [a] -> ( a -> [a] ) -> [a]
hull x0s f = 
    let step done todo = case S.minView todo of
            Nothing -> []
            Just (t, odo) -> 
                if S.member t done
                then step done odo
                else t : step (S.insert t done)
                              (S.union (S.fromList $ f t) odo)
    in  step S.empty $ S.fromList x0s

ctrans = complete trans
top = maximum $ M.elems ctrans

apply (Left o) s = maybeToList $ M.lookup (o,s) ctrans 
apply (Right o) s = maybeToList $ M.lookup (s,o) ctrans 

mirror_pairs = filter ( \ m -> mirror (from m) (to m) ) pairs

mirror (a,b) (c,d) = a == d && b == c

pairs = pairs_from units

fromzero = do 
    p <- [ 0 ] ; q <- [ p+1 .. top ] 
    return $ Move {from=(p,q), by = [], to=(p,q) } 

fromnormalizing = do 
    p <- [ 0 .. top - 1 ] ; q <- [ p+1 .. top ] 
    return $ Move {from=(p,q), by = [], to=(p,q) } 


units = do 
    p <- [ 0 .. top ] ; q <- [ p+1 .. top ] 
    return $ Move {from=(p,q), by = [], to=(p,q) } 


pairs_from start  = hull start $ \ m -> do
       o <- [ 0 .. top ]
       c <- [ Left o, Right o ]
       p' <- apply c $ fst $ to m
       q' <- apply c $ snd $ to m
       return $ m { to = (p',q'), by = c : by m }

    
