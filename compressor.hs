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
    -- let ts = concat $ terms_for [s] 
    let ts = map monster [ 1 ..  ]
    forM_ ts $ \ t -> 
        when ( normalizing t ) $ do
            let (s,r,e) = normal t
            (up, this) <- atomically $ do
                let this = fromIntegral s % size t
                prev <- readTVar top
                writeTVar top $ max this prev
                return ( this >= prev, this )
            when up $ do
                print ( (fromRational this) :: Double
                      , t,size t
                      , size e, s)
                hFlush stdout

line k = 
    unspine $ replicate (k + 1) s
monster k = 
    unspine [ s, unspine [s,s,s], line k, s, s, s]

