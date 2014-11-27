import S.Type
import S.Size
import S.Model

import qualified S.ModelIO

import S.ToDoc
import S.Reduce (isnormal)
import qualified S.Reduce 
import S.Table
import qualified S.TableHead as TH
import qualified S.Normal
import qualified S.Head
import S.Verify
import S.Back
import L.Type ( froms )
import qualified L.Reduce
import qualified L.Eval
import qualified L.Eval.Pure
import qualified L.Eval.Monadic as M
import qualified L.Eval.Generic as G
import qualified L.Convert
import qualified L.Pure

import Control.Monad ( forM_, when, guard )
import Data.List ( inits, tails, nub)
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Concurrent.STM
import Data.Maybe (isNothing, isJust, maybeToList)
import System.IO 

main = do

    find_normal_monster_for [b,d]
    -- find_divergent_for [b,d]
    -- write_cl_table_for [ d,b ] 6 500

    -- this does not seem to halt:
    -- write_beta_table 10 1000

    -- this terminates (38 states):
    -- write_cl_table 6 200



    -- find_max_size $ 10^1

    -- S.Model.write_full_black_table 
    -- compare_cl_beta_normal

    -- find_max_left
    -- find_max_order 
    -- L.Convert.find_convertible_normalforms
    -- L.Pure.find_pure
    -- write_beta_table 


    -- let second = 10^6 ; minute = 60 * second
    -- L.Eval.find_monster_to $ 15 * minute 
    -- S.Normal.find_monster


    -- S.Reduce.find_maxpipe
    -- L.Reduce.find_deep_vars
    -- L.Reduce.find_weak_strong
    -- print_multi_origins
    -- local S.Table.trans
    -- local_head TH.trans

    -- write_head_table
    -- check_head_normalization
    -- find_head_monster

    -- check_forward_closed_head TH.trans
    -- equiv_examples 

compare_cl_beta_normal = forM_ ( concat $ S.Type.normalforms ) $ \ t -> do
    let d = 500
        b = M.eval d t
        c = S.Normal.normal d $ unspine [ t, var 1, var 2, var 3]
    if (isJust b == isJust c) 
       then do putStr "." ; hFlush stdout
       else printf (t, isJust b, isJust c)

find_max_left = do
    let work top (t:ts) = 
            case M.eval 500 t of
               Just e -> do
                let (h,m) = G.eval G.max_left_depth t
                when (m > top) $ do
                    print (t, e, m)
                    -- print $ last $ L.Reduce.imo $ froms t
                    -- putStrLn $ take 10 $ show $ last $ L.Reduce.imo $ froms t
                    hFlush stdout
                work (max top m) ts
               Nothing -> work top ts
    work 0 $ concat S.Type.normalforms

find_max_order = do
    let work top (t:ts) = 
            case M.eval 500 t of
               Just e -> do
                let (h,m) = G.eval G.max_lambda_depth t
                when (m >= top) $ do
                    print (t, e, (h,m))
                    -- print $ last $ L.Reduce.imo $ froms t
                    -- putStrLn $ take 10 $ show $ last $ L.Reduce.imo $ froms t
                    hFlush stdout
                work (max top m) ts
               Nothing -> work top ts
    work 0 $ concat S.Type.normalforms

find_max_size mu = do
    let work top (t:ts) = do
            ms <- L.Eval.Pure.eval_musec mu t
            case ms of
               Just s -> do
                when (s >= top) $ do
                    print (t, size t, s)
                    hFlush stdout
                work (max top s) ts
               Nothing -> work top ts
    -- work 0 $ concat S.Type.normalforms
    work 0 $ concat S.Type.terms

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

find_divergent_for bs = do
    div <- atomically $ newTVar $ S.empty
    forM_ (concat $ terms_for bs) $ \ t -> do
        s <- atomically $ readTVar div
        when ( not $ or $ for (subterms t) $ \ u -> S.member u s 
              || case u of App {fun=D,arg=App{fun=App{fun=B},arg=D}} -> True ; _ -> False
               ) $ do
            case S.Normal.normal (10 ^ 2) t of
                Nothing -> do
                    atomically $ modifyTVar div $ S.insert t
                    -- printf t
                Just n -> do
                    printf t
                    return ()

for = flip map

find_normal_monster = do
    top <- atomically $ newTVar 0
    forM_ (concat $ terms) $ \ t -> 
        when (S.Table.normalizing t) $ do
            let n = S.Normal.normalform t
            up <- atomically $ do
                s <- readTVar top
                let up = size n > s 
                when up $ writeTVar top $ size n
                return up
            when up $ printf (size t, size n, t)

find_normal_monster_for bs = do
    top <- atomically $ newTVar 0
    forM_ (concat $ terms_for bs) $ \ t -> 
        case S.Normal.normal (10 ^ 2) t of
          Nothing -> return ()
          Just n -> do
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

write_cl_table = write_cl_table_for [s] 

write_cl_table_for bs dep len = do
    m0 <- model_for bs dep len
    m1 <- build_full_for bs m0
    print $ toDoc $ base m1
    print $ toDoc $ S.Model.trans m1
    print $ toDoc $ S.Model.accept m1

write_head_table = do
    m0 <- model0 8 200
    m1 <- build_head m0
    print $ toDoc $ base m1
    print $ toDoc $ S.Model.trans m1
    print $ toDoc $ S.Model.accept m1

write_beta_table depth mu = do
    m0 <- S.ModelIO.model0 depth mu
    m1 <- S.ModelIO.build_beta_full m0
    print $ toDoc $ S.ModelIO.base m1
    print $ toDoc $ S.ModelIO.trans m1
    print $ toDoc $ S.ModelIO.accept m1


----------------------------------------------------

highdees n k = do
    yield <- inserts k d $ replicate n b
    t <- termsfrom yield
    return t

looksnormalizing n = isJust . S.Normal.normal n

-- replace exactly k elements of ys by x    
inserts :: Int -> a -> [a] -> [[a]]
inserts 0 x ys = return ys
inserts k x ys = do
    guard $ k <= length ys
    (pre, this : post) <- splits ys
    post' <- inserts (k-1) x post
    return $ pre ++ x : post'

termsfrom :: [T] -> [T]
termsfrom [leaf] = return leaf
termsfrom yield = do
    (pre, post) <- splits yield
    guard $ not $ null pre
    guard $ not $ null post
    l <- termsfrom pre ; r <- termsfrom post
    return $ app l r

splits xs = zip (inits xs) (tails xs)

-- | ground term over base, attach variables [1..k],
-- enumerate all pure subterms that are reachable
reachable_subterms_for bs k = nub $ do
    n <- [1 .. ]
    t <- terms_for bs !! n
    let s = unspine $ t : map var [ 1 .. k ]
    n <- maybeToList $ S.Normal.normal 100 s
    u <- subterms n
    guard $ bfree u 
    return u

bfree u = and $ for (subterms u) $ \ s -> s /= b
