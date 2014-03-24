module L.Eval where

import L.Eval.Monadic ( eval )

import qualified S.Type as S
import qualified S.Table as S

import Control.Concurrent.STM

import Control.Monad ( forM_, when )
import System.IO 

find_monster = do
    top <- atomically $ newTVar 0
--    forM_ (concat S.terms) $ \ t -> do   
    forM_ (concat S.normalforms) $ \ t -> do   
        best <- atomically $ readTVar top
        case eval (10^4) t of
            Nothing -> when True $ printf (t, "*")
            Just (n,s) -> when ( n >= best ) $ do
                printf (t,(n,s))
                atomically $ writeTVar top n

printf x = do print x ; hFlush stdout

