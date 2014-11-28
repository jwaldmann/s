{-

notation: 

t in CL(S), 
Nf(t) - normal form of t
DAG(t) - DAG of t with complete exact hash consing
(equivalently, reduced in the BDD sense: each node has an address, 
for each addresses p, q: @(p,q) occurs at most once)

conjectures:




-}


import S.DAG
import S.Type
import S.Size
import S.Table (normalizing)

import Data.Ratio
import Control.Concurrent.STM
import Control.Monad ( when, forM_ )
import System.IO

main = do
    top <- atomically $ newTVar 0
    let ts = -- concat $ terms_for [s] 
             -- map monster2 [ 1 ..  ]
             map monster3 [ 1 ..  ]
    forM_ ts $ \ t -> 
        when ( normalizing t ) $ do
            let (s,o) = normal t
            (up, this) <- atomically $ do
                let this = -- fromIntegral s % size t
                         fromIntegral o % fromIntegral s
                         -- fromIntegral o % size t
                prev <- readTVar top
                writeTVar top $ max this prev
                return ( this >= prev, this )
            when up $ do
                print ( (fromRational this) :: Double
                      , t,size t
                      -- , size e
                      , s)
                hFlush stdout

line k = 
    unspine $ replicate (k + 1) s
monster1 k = 
    unspine [ s, a, line k, s, s, s]
monster2 k = 
    unspine [ s, t, t, line k, s]
monster3 k = 
    let tee k = foldr app (app s a) $ replicate k t
    in  unspine [ tee k,s,s,s]
