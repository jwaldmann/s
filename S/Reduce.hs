module S.Reduce where

import S.Type

import Control.Monad ( forM_, when )
import Control.Applicative ( (<$>) )

import System.IO

normalize steps t = do
    let (pre,post) = splitAt steps $ innermost t
    return $ if null post then Just $ last pre else Nothing


leftmost :: T -> [T]
leftmost t = t : case next t of
    [] -> []
    x : _ -> leftmost x

next :: T -> [T]
next t = here t ++ ( case t of
    App {fun=x,arg=y} -> 
        map (\ x' -> app x' y) (next x)
     ++ map (\ y' -> app x y') (next y)
    _ -> []
         ) 

innermost t = t : case next_inner t of
    [] -> []
    x : _ -> leftmost x


next_inner t = ( case t of
    App {fun=x,arg=y} -> 
        map (\ x' -> app x' y) (next x)
     ++ map (\ y' -> app x y') (next y)
    _ -> []
  ) ++  here t 
         

here t = case t of
    App{fun=App{fun=App{fun=S,arg=x},arg=y},arg=z} ->
        [ app (app x z) (app y z) ]
    _ -> []

isnormal = null . next


pipe :: T -> Int
pipe t = case t of
    App{fun=f,arg=a} | f == s -> succ $ pipe a
    _ -> 0

maxpipe t = maximum $ do s <- subterms t ; return $ pipe s

find_maxpipe = forM_ ( concat terms ) $ \ t -> do
    let (pre,post) = splitAt 100 $ leftmost t
        m = maximum $ pipe <$> ( pre >>= subterms )
    when (maxpipe t + 3 <= m) $ do
        print (t, maxpipe t, m )
    hFlush stdout
