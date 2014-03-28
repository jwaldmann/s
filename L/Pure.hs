module L.Pure where

import qualified L.Type as S
import qualified S.Type as S

import L.Eval.Generic ( pure )
import L.Eval.Monadic ( eval )
import L.Reduce (imo)

import Control.Monad ( forM_, when )
import qualified Data.Map.Strict as M
import System.IO

find_pure = do
    forM_ ( concat S.normalforms ) $ \ t -> do
        case eval (10^3) t of
            Nothing -> do
                -- putStr "!" ; hFlush stdout
                return ()
            Just _ -> do
                when (pure t) $ do
                    print t
                -- putStr "." ; hFlush stdout


