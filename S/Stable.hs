module S.Stable where

import S.Type

import qualified S.TableHead as H
import qualified S.Head as H


-- | head reductions until the root is stable
-- because we either reach head normal form,
-- or the left child has no head normal form.
-- (stability can always be achieved because of top termination)
stable :: T -> T
stable t = 
    let ts = H.plain t
    in  if H.normalizing t then last ts
        else head $ filter ( \ App{fun=f,arg=a} -> not $ H.normalizing f ) ts

