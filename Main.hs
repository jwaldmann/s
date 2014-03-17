import S.Type
import S.Size
import S.Model
import S.ToDoc
import S.Reduce (isnormal)
import S.Table
import qualified S.TableHead as TH
import qualified S.Normal
import qualified S.Head
import S.Verify
import S.Back
import qualified L.Reduce

import Control.Monad ( forM_, when )
import qualified Data.Map as M
import Control.Concurrent.STM
import Data.Maybe (isNothing)
import System.IO 

main = do
    L.Reduce.find
    -- print_multi_origins
    -- local S.Table.trans
    -- local_head TH.trans

    -- write_head_table
    -- check_head_normalization
    -- find_head_monster
    -- find_normal_monster

    -- check_forward_closed_head TH.trans
    -- equiv_examples 

print_multi_origins = forM_ multi_origins $ \ (t,os) -> do
     print (t, toDoc os)


equiv_examples = do
    let handle m (t:ts) = do
            let v = TH.value t
            case M.lookup v m of
                Nothing -> return ()
                Just others -> print (v,  toDoc $ t : others)
            handle (M.insertWith (++) v [t] m) ts
    handle M.empty $ filter isnormal $ concat terms

check_normalization = do
    forM_ (concat terms) $ \ t -> do
        let v = value t
            n = S.Normal.normal 100 t
        if isNothing v == isNothing n then putStr "." else error $ show $ toDoc (t,v,n)
        hFlush stdout

check_head_normalization = do
    forM_ (concat terms) $ \ t -> do
        let v = TH.normalizing t
            n = null $ drop 100 $ S.Head.plain t
        if v == n then if v then putStr "." else putStr "!" else error $ show $ toDoc (t,v,n)
        hFlush stdout

find_normal_monster = do
    top <- atomically $ newTVar 0
    forM_ (concat terms) $ \ t -> 
        when (S.Table.normalizing t) $ do
            let n = S.Normal.normalform t
            up <- atomically $ do
                s <- readTVar top
                let up = size n > s 
                when up $ writeTVar top $ size n
                return up
            when up $ printf (size t, size n, t)

find_head_monster = do
    top <- atomically $ newTVar 0
    forM_ (concat terms) $ \ t -> 
        when (TH.normalizing t) $ do
            let ts = S.Head.plain t
            up <- atomically $ do
                s <- readTVar top
                let up = length ts > s 
                when up $ writeTVar top $ length ts
                return up
            when up $ printf (size t, length ts, size $ last ts, t)

write_head_table = do
    m0 <- model0 8 200
    m1 <- build_head m0
    print $ toDoc $ base m1
    print $ toDoc $ S.Model.trans m1
    print $ toDoc $ S.Model.accept m1
