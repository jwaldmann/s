module L.Convert where

import qualified L.Type as L
import qualified S.Type as S

import L.Reduce (imo)

import S.Normal ( normal )

import qualified Data.Map.Strict as M
import System.IO

find_convertible_normalforms = do
    let 
        work m  (t:ts) = case nf t of
            Nothing -> do
                -- putStr "!" ; hFlush stdout
                work m ts
            Just n -> do
                -- print (t, n)
                -- putStr "." ; hFlush stdout
                case M.lookup n m of
                    Nothing -> return ()
                    Just t0 -> do
                        putStr "\nEQ: "
                        print (t0, t)

                        extract_context_saturate (t0,t)

                        -- print n
                work (M.insert n t m) ts
    work M.empty $ concat $ drop 10 $ S.normalforms


extract_context_saturate (t0,t1) = do
    case (t0,t1) of
        (S.App {S.fun=f0,S.arg=a0}, S.App {S.fun=f1,S.arg=a1}) -> 
            if f0 == f1 then do
                putStrLn "common left context"
                extract_context_saturate (a0,a1)
            else if a0 == a1 then do
                putStrLn "common right context"
                extract_context_saturate (f0,f1)
            else compare_saturate 1 (t0,t1)


compare_saturate i (s,t) | i < 5 = do
    putStr "compare" ; print (s,t)
    let [a,b] = map ( normal 1000 ) [ s,t]
    case (a,b) of
        (Just x, Just y) -> 
            if x == y 
            then putStrLn "have identical CL-nfs"
            else compare_saturate (i+1) (S.app s $ S.var i, S.app t $ S.var i)
        _ -> putStrLn "diverges"
compare_saturate _ _ = putStrLn "cannot eat argument"


nf t = case splitAt 70 $ imo $ L.froms t of
            ( ts, [] ) -> Just $ last ts
            _ -> Nothing
