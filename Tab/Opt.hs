-- | find a decision tree representation 
-- for M.Map (Int,Int) Int
-- by evolutionary optimisation

{-# language OverloadedStrings #-}
   
module Tab.Opt where

import Tab.Exp
import Tab.Bin (from)  

import qualified Data.Map as M
import Control.Monad
import System.Random
import Data.Function (on)
import Data.List (sortBy)
import System.IO

data Form = Form { geno :: [ Bool ]
                 , pheno :: Exp
                 , penalty :: Int
                 }
     deriving Show

bad = 1000

work target bitsize popsize = do
    fs <- replicateM popsize $ roll target bitsize
    let handle mbest fs = do
            let w = 10 :: Int
            fs' <- permute w fs
            let (gs,hs) = splitAt w fs'
            children <- replicateM (w*w) $ do
              f:g:h:i:_ <- permute (4::Int) gs
              fg <- cross2 (geno f) (geno g)
              hi <- cross2 (geno h) (geno i)
              m <- cross2 fg hi
              m <- mutate m
              m <- mutate m    
              return $ form target m
            let part = take w
                     $ sortBy (compare `on` penalty)
                     $ children ++ gs
                p = head part
                (printing, mbest') = case mbest of
                    Nothing -> (True, Just $ penalty p)
                    Just best ->
                      ( penalty p < best
                      , Just $ min best $ penalty p )
            when printing $ print p
            hPutStr stderr $ show (penalty p) ++ " "
            handle mbest' $ part ++ hs            
    handle Nothing fs

mutate xs = do
    i <- randomRIO (0, length xs - 1)
    let (pre, this : post) = splitAt i xs
    return $ pre ++ not this : post

cross2 xs ys = do
    i <- randomRIO (0, length xs - 1)
    j <- randomRIO (0, length xs - 1)
    let lo = min i j ; hi = max i j
    return $ take lo xs ++ take (hi - lo) ys ++ drop hi xs
    
cross xs ys = do
    i <- randomRIO (0, length xs)
    return $ take i xs ++ drop i ys
    
permute 0 xs = return xs
permute k xs = do
    s <- randomRIO (0, length xs - 1)
    let (pre, this : post) = splitAt s xs
    ys <- permute (k-1) $ pre ++ post
    return $ this : ys
    
roll target s = do
    bits <- replicateM s $ randomRIO (False,True)
    return $ form target bits
    
form :: ( M.Map (Int,Int) Int )
     -> [Bool]
     -> Form
form target bits =
    let e = Tab.Bin.from target bits
        f = value e
        n = succ $ maximum $ M.elems target
        p = sum $ do
           x <- [ 0 .. n-1 ]
           y <- [ 0 .. n-1 ]
           return $ signum
                  $ abs $ f (x,y) - target M.! (x,y)
    in  Form { geno = bits, pheno = e
             , penalty = -- if p > 0 then error "huh" else
                         size e
             }
    
