module S.Context where

import S.Type
import S.Size

data Context = This | L !T !Context | R !Context !T
    deriving ( Eq, Ord, Show )

instance Size Context where
    size c = case c of
        This -> 1 ; L t c -> succ $ size t + size c ; R c t -> succ $ size c + size t

contexts = contexts_for [s]

contexts_for :: [T] -> Int -> [Context]
contexts_for base s = if s <= 0 then [ This ] else do
    here <- [1..s]
    c <- contexts_for base ( s - here )
    t <- terms_for base !! here
    [ L t c, R c t ]

plugin :: Context -> T -> T
plugin con t = case con of
    This -> t
    L x c -> app x (plugin c t)
    R c y -> app (plugin c t) y

