-- | unfailing binary parser,
-- will construct a correct (!) expression

module Tab.Bin where

import Prelude hiding ( exp )       
import Tab.Exp
import Text.Parsec 
import Control.Applicative ((<*>),(<$>))
import Control.Monad ( guard )

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List ( nub )

type Table = M.Map (Int,Int) Int

from :: Table -> [ Bool ] -> Exp
from tab s = case parse (exp tab) "bin" s of
    Right e -> e

exp tab = case nub $ M.elems tab of
    [] -> return $ Const (-42)
    [ c ] -> return $ Const c
    _ -> do
        let candidates = do
              p <- [X, Y]
              let is = nub $ map (get p) $ M.keys tab
              guard $ length is > 1
              return (p, is)
        (p, is) <- pick candidates
        i <- pick is
        let prop = \ k -> get p k == i
        IfEq p i <$> exp (M.filterWithKey (const . prop) tab)
                 <*> exp (M.filterWithKey (const . not . prop) tab)

pick xs = (xs !!) <$> number (length xs)

position = (toEnum :: Int -> Pos) <$> number 2
    
-- | return a number in [ 0 .. n-1 ]
number 1 = return 0
number n | n > 1 = do
    b <- next
    x <- number $ div n 2
    return $ mod ( 2*x + fromEnum b ) n

next = tokenPrim
     ( \ x -> show x )
     ( \ pos x xs -> pos )
     ( \ x -> Just x )
   <|> return False
    

