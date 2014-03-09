module S.Verify where

import S.Type
import S.Table

import qualified Data.Map as M
import Control.Monad ( sequence_, forM_, when )
import Data.List ( intersperse )

local t = do
    let app p q = t M.! (p,q)
        i = invert t
        inputs p = M.findWithDefault [] p i
    let mk l r s t = 
            let f = concat $ intersperse "_" [ "A", show l, show r ]
            in  f ++ "(" ++ s ++ "," ++ t ++ ")"
        es p = "S_" ++ show p
        var v = v
    putStrLn "(VAR x y z) (RULES"
    forM_ ( M.keys i ) $ \ p ->
        forM_ (inputs p) $ \ (q,z) ->
            forM_ (inputs q) $ \ (r,y) ->
                forM_ (inputs r) $ \ (s,x) ->
                    when (0 == s) $ do
                        let xz = app x z ; yz = app y z
                            reduct = app xz yz
                        when (p /= reduct) $ error $ show (p, reduct)
                        putStrLn $ unwords [ mk q z (mk r y (mk s x (es 0) (var "x")) (var "y")) (var "z")
                                           , "->"
                                           , mk xz yz (mk x z (var "x") (var "z")) (mk y z (var "y") (var "z"))
                                           ]
    putStrLn ")"


local_head t = do
    let app p q = t M.! (p,q)
        i = invert t
        inputs p = M.findWithDefault [] p i
    let mk l r s t = 
            let f = concat $ intersperse "_" [ "A", show l, show r ]
            in  f ++ "(" ++ s ++ "," ++ t ++ ")"
        es p = "S_" ++ show p
        var v = v
    putStrLn "(VAR x y z) (RULES"
    forM_ ( filter (/= 34) $ M.keys i ) $ \ p ->
        forM_ (inputs p) $ \ (q,z) ->
            forM_ (inputs q) $ \ (r,y) ->
                forM_ (inputs r) $ \ (s,x) ->
                    when (0 == s) $ do
                        let xz = app x z ; yz = app y z
                            reduct = app xz yz
                        when (p /= reduct) $ error $ show (p, reduct)
                        putStrLn $ unwords [ mk q z (mk r y (mk s x (es 0) (var "x")) (var "y")) (var "z")
                                           , "->"
                                           , mk xz yz (mk x z (var "x") (var "z")) (mk y z (var "y") (var "z"))
                                           ]
    putStrLn ")"


local_head_simplified t = do
    let app p q = t M.! (p,q)
        i = invert t
        inputs p = M.findWithDefault [] p i
    let mk l r s t = 
            let f = concat $ intersperse "_" [ "A", show l , show r ]
            in  f ++ "(" ++ s ++ {- "," ++ t ++ -} ")"
        es p = "S_" ++ show p
        var v = v
    putStrLn "(VAR x y z) (RULES"
    forM_ ( filter (== 34) $ M.keys i ) $ \ p ->
        forM_ (inputs p) $ \ (q,z) ->
            forM_ (inputs q) $ \ (r,y) ->
                forM_ (inputs r) $ \ (s,x) ->
                    when (0 == s) $ do
                        let xz = app x z ; yz = app y z
                            reduct = app xz yz
                        when (p /= reduct) $ error $ show (p, reduct)
                        putStrLn $ unwords [ mk q z (mk r y (mk s x (es 0) (var "x")) (var "y")) (var "z")
                                           , "->"
                                           , mk xz yz (mk x z (var "x") (var "z")) (mk y z (var "y") (var "z"))
                                           ]
    putStrLn ")"


invert t = M.fromListWith (++) $ do
    (k,v) <- M.toList t
    return (v, [k])

check_forward_closed t = do
    let c = complete trans
        app p q = c M.! (p,q)
        i = invert c
        inputs p = M.findWithDefault [] p i
    forM_ ( M.keys i ) $ \ p ->
        forM_ (inputs p) $ \ (q,z) ->
            forM_ (inputs q) $ \ (r,y) ->
                forM_ (inputs r) $ \ (s,x) ->
                    when (0 == s) $ do
                        let reduct = app (app x z) (app y z)
                        when (p /= reduct) $ error $ show (p, reduct)
                        putStr $ show p ++ " "
    putStrLn "is forward-closed"

check_forward_closed_head t = do
    let app p q = t M.! (p,q)
        i = invert t
        inputs p = M.findWithDefault [] p i
    forM_ ( M.keys i ) $ \ p ->
        forM_ (inputs p) $ \ (q,z) ->
            forM_ (inputs q) $ \ (r,y) ->
                forM_ (inputs r) $ \ (s,x) ->
                    when (0 == s) $ do
                        let reduct = app (app x z) (app y z)
                        when (p /= reduct) $ error $ show (p, reduct)
                        putStr $ show p ++ " "
    putStrLn "is forward-closed-head"


check_backward_closed t = do
    let c = complete trans
        app p q = c M.! (p,q)
        i = invert c
        inputs p = M.findWithDefault [] p i
    forM_ ( M.keys i ) $ \ p ->
        forM_ (inputs p) $ \ (xz,yz) ->
            forM_ (inputs xz) $ \ (x,z) ->
                forM_ (inputs yz) $ \ (y,z') ->
                    when (z == z') $ do
                        let redex = app (app (app 0 x) y) z
                        when (p /= redex) $ error $ show (p, redex)
                        putStr $ show p ++ " "
    putStrLn "is backward-closed"

    
