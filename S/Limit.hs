module S.Limit where

import S.Type

import S.Stable ( stable )

import qualified S.Head as H
import qualified S.TableHead as H

import qualified S.Normal as F
import qualified S.Table as F

import Data.Tree

-- | this tree is: N normal form, 
--  H: not normal, but head normal
--  U: unsolvable (no head normal form)
data Info = N | H | U deriving ( Eq, Ord, Show )

limit :: Int -> T -> Tree Info
limit depth t = 
    let s = stable t
        lab = if F.normalizing s then N 
              else if H.normalizing s then H
              else U
    in  case s of
             App {fun=f,arg=a} | depth > 0 -> 
                 Node lab [ limit (depth-1) f, limit (depth-1) a]
             _ -> Node lab []
