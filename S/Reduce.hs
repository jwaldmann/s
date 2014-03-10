module S.Reduce where

import S.Type

leftmost :: T -> [T]
leftmost t = t : case next t of
    [] -> []
    x : _ -> leftmost x

next :: T -> [T]
next t = here t ++ ( case t of
    S -> []
    App {fun=x,arg=y} -> 
        map (\ x' -> app x' y) (next x)
     ++ map (\ y' -> app x y') (next y)
         ) 

here t = case t of
    App{fun=App{fun=App{fun=S,arg=x},arg=y},arg=z} ->
        [ app (app x z) (app y z) ]
    _ -> []

isnormal = null . next
