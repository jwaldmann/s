module L.Eval where

import L.Eval.Monadic ( eval )
import L.Eval.Pure ( eval_musec )
import L.Type ( froms )

import L.Reduce (imo)

import qualified S.Type as S
import qualified S.Table as S

import Control.Concurrent.STM

import Control.Monad ( forM_, when )
import System.IO 

normalize steps t = do
    case eval steps t of
        Just (n,s) -> return $ Just t
        Nothing -> return Nothing

find_monster_to bound = do
    top <- atomically $ newTVar 0
    forM_ (concat $ S.terms_for [S.J]) $ \ t -> do   
--    forM_ (concat S.normalforms) $ \ t -> do   
        best <- atomically $ readTVar top
        -- let res = eval bound t
        ms <- eval_musec bound t
        let res = do m <- ms ; return (m,Nothing :: Maybe Integer)
        case res of
            Nothing -> when True $ printf (t, "*")
            Just (n,s) -> when ( n >= best ) $ do
                printf (t,(n,s))
                -- print $ last $ imo $ froms t
                atomically $ writeTVar top n

printf x = do print x ; hFlush stdout

