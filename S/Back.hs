module S.Back where

import S.Type
import S.Table (normalizing)
import S.Reduce (isnormal)

import S.Gen (nono)
import Data.List (nub)
import Control.Monad ( guard )


multi_origins = do 
    t <- nono 
    let os = filter subnormal $ nub $ origins t
    guard $ not $ null $ drop 1 os 
    return (t,os)

-- | alle Teilterme, die eine Normalform haben,
-- sollen in Normalform sein
subnormal t = if normalizing t then isnormal t 
    else case t of
        App{fun=f,arg=a} -> all subnormal [f,a]


back = everywhere back_here

everywhere h t = h t ++ case t of
    S -> []
    App {fun=f,arg=a} -> 
         map (\f' -> app f' a) (everywhere h f)
      ++ map (\a' -> app f a') (everywhere h a)

back_here t = case t of
    App{fun=App{fun=x,arg=z},arg=App{fun=y,arg=z'}} | z==z'
        -> [ app (app (app s x) y) z ]
    _ -> []

origins t = case back t of
    [] -> [t]
    ts -> concat $ map origins ts
