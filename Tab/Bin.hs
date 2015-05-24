-- | unfailing binary parser,
-- will construct a correct (!) expression

module Tab.Bin where

import Prelude hiding ( exp )       
import Tab.Exp
import Text.Parsec 
import Control.Applicative ((<*>),(<$>))
import Control.Monad ( guard )

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List ( nub, sortBy, groupBy )
import Data.Function (on)

import Control.Parallel.Strategies       
              
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
              let js = id
                     -- $ head $ groupBy ( (==) `on` third )
                     -- $ sortBy (compare `on` third )
                     -- $ ( \ xs -> xs `using` parBuffer 8 (evalTuple3 rseq rseq rseq) )
                     $ do
                    i <- is
                    let prop = \ k v -> get p k == i
                        (yeah,noh) = M.partitionWithKey prop tab
                        d = (+) (diversity yeah) (diversity noh)
                    return (i, (yeah,noh), d)
              return (p, js)
        (p, is) <- pick candidates
        (i, (yeah,noh), d) <- pick is
        IfEq p i <$> exp yeah <*> exp noh

third (x,y,z) = z    
diversity tab = S.size $ S.fromList $ M.elems tab
    
pick xs = (xs !!) <$> number (length xs)

position = (toEnum :: Int -> Pos) <$> number 2
    
-- | return a number in [ 0 .. n-1 ]
number 1 = return 0
number n | n > 1 = do
    b <- next
    x <- number $ div (succ n) 2
    return $ mod ( 2*x + fromEnum b ) n

next = tokenPrim
     ( \ x -> show x )
     ( \ pos x xs -> pos )
     ( \ x -> Just x )
   <|> return False
    

