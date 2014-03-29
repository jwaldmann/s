module L.Convert where

import qualified L.Type as S
import qualified S.Type as S

import L.Reduce (imo)

import qualified Data.Map.Strict as M
import System.IO

find_convertible_normalforms = do
    let 
        work m  (t:ts) = case nf t of
            Nothing -> do
                putStr "!" ; hFlush stdout
                work m ts
            Just n -> do
                -- print (t, n)
                putStr "." ; hFlush stdout
                case M.lookup n m of
                    Nothing -> return ()
                    Just t0 -> do
                        print (t0, t, n)
                work (M.insert n t m) ts
    work M.empty $ concat S.normalforms

nf t = case splitAt 70 $ imo $ S.froms t of
            ( ts, [] ) -> Just $ last ts
            _ -> Nothing
