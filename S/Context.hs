module S.Context where

import S.Type

data Context = This | L !T !Context | R !Context !T
    deriving ( Eq, Ord, Show )

contexts :: Int -> [Context]
contexts s = if s <= 0 then [ This ] else do
    here <- [1..s]
    c <- contexts ( s - here )
    t <- terms !! here
    [ L t c, R c t ]

plugin :: Context -> T -> T
plugin con t = case con of
    This -> t
    L x c -> app x (plugin c t)
    R c y -> app (plugin c t) y

