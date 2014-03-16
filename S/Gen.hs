module S.Gen where

import S.Type
import S.Table 
import qualified Data.Map as M
import Data.List (nub, inits)
import Data.Maybe (isJust)
import Control.Applicative ( (<$>), (<*>) )

invert :: M.Map (Int,Int) Int -> M.Map Int [Maybe (Int,Int)]
invert m = M.fromListWith (++) $ (0, [Nothing]) : do
    ((p,q),r) <- M.toList m
    return (r, [ Just (p,q) ])

au = invert $ complete trans

type TS = [[T]] -- | by increasing size, starting with 0

-- | for each state, a lazy list of terms
-- that are accepted in that state.
generate :: M.Map Int  [Maybe (Int,Int)]
         -> M.Map Int [[T]]
generate au = 
    let m = M.fromList $ do
            r <- M.keys au
            let series = unions $ do
                    v <- au M.! r
                    return $ case v of
                        Nothing -> [ [s]]
                        Just (p,q) -> 
                           [] : crosses app (m M.! p) (m M.! q)
            return (r, series)
    in  m

unions = foldr union []

union xs [] = xs
union [] ys = ys
union (xs:xss) (ys:yss) = 
    nub (xs ++ ys) : union xss yss

crosses f =  cross ( \xs ys -> f <$> xs <*> ys )

cross :: Eq c => (a->b->[c]) -> [a] -> [b] -> [[c]]
cross f xs ys = 
    let lift xs = map Just xs ++ repeat Nothing
        unlift (Just x:ys) = x ++ unlift ys
        unlift (Nothing:ys) = unlift ys
        unlift _ = []
    in  map (nub . unlift)
       $ takeWhile ( any isJust )
       $  cross_inf (\ x y -> f <$> x <*> y)
        ( lift xs) (lift ys)

cross_inf ::  (a->b->c) -> [a] -> [b] -> [[c]]
cross_inf f xs ys = do
    xs' <- tail $ inits xs
    return $ zipWith f (reverse xs') ys



