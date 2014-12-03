import S.Type
import S.Head (plain)
import Control.Monad (guard )

data Run = This Int | FromTo Int Int

instance Show Run where
    show (This i) = show i
    show (FromTo lo hi) = show lo ++ " .. " ++ show hi

run lo hi  = if lo + 5 < hi then [ FromTo lo hi ] else map This [lo .. hi ]

runs :: [ Int ] -> [ Run ]
runs (x:xs) =
    let (c,_,ys) = extract_common [x..] (x:xs)
    in  run (head c) (last c) ++  runs ys

extract_common (x:xs)(y:ys) = 
    if x == y then let (c,l,r) = extract_common xs ys in (x : c,l,r)
    else ([], x:xs, y:ys)

increasing (x:xs) = x : increasing (filter (> x) xs)

main = mapM_ print $ increasing $ do 
    t <- concat $ terms_for [s] 
    let { ts = drop 10000 $ plain t } 
    guard $ not $ null ts 
    return (length $ spine $ head ts, t ) 
