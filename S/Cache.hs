module S.Cache where

import S.Type (T)
import qualified Data.Map.Strict as M

data Cache = Cache { m :: M.Map T (Maybe T) , st :: Int }

