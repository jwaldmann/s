import L.Reduce (imo)
import L.Type 
import S.Data (terms_for, T(..))
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (sortBy)
import Data.Function (on)
import Control.Monad ( guard )
import System.Environment

main = main2

--------------------------------------------------

main1 = do
  [k] <- getArgs
  let ds = map absdepth $ path $ bee $ read k
  mapM_ print $ increasing $ zip ds $ map negate [0..]

nf = last . imo

up k x =
  if k > 1
  then app (app the_b (up (k-1) x)) x
  else x

bee k = nf $ app (up k $ the_b) the_b

path x = scanl (\ b a -> nf $ app b a ) x $ repeat x

increasing (x:xs) = x :increasing (filter (>x) xs)


----------------------------------------------------

main2 = do
  [n] <- getArgs
  print $ represent $ rightspine $ read n

represent t = head $ do
  s <- concat $ terms_for [B]
  guard $ t == ( nf $ froms  s)
  return (s,t)

rightspine k =
  let b = foldr app (var 0) $ map var $ reverse [ 1 .. k -1 ]
  in  iterate lam b !! k

not_representable s = do
  let c = S.fromList $ do k <- [ 1.. s] ; candidates k
      d = foldl ( \ c t -> S.delete t c ) c
        $ map nf $ map froms $ concat $ take s $ terms_for [B]
  k <- [ 1 ..s ]
  return $ S.filter (\t -> size t == 2 * fromIntegral k) d

candidates s = map (\ t -> iterate lam t !! s) $ make s

make s =
  if s > 1
  then do
    sl <- [ 1 .. s -1 ] ; l <- make sl
    let { sr = s - sl } ; r <- make sr
    return $ app (varmap (sr +) l) $  r
  else [ var 0 ]
      
