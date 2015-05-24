import L.Reduce (imo)
import L.Type 

import System.Environment

main = do
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

