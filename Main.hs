import S.Type
import S.Model
import S.ToDoc
import S.Table ( value )
import S.Normal

import Control.Monad ( forM_ )
import Data.Maybe (isNothing)
import System.IO 

main = check_normalization

check_normalization = do
    forM_ (concat terms) $ \ t -> do
        let v = value t
            n = normal 100 t
        if isNothing v == isNothing n then putStr "." else error $ show $ toDoc (t,v,n)
        hFlush stdout

write_table = do
    m0 <- model0 5 100
    m1 <- build m0
    print $ toDoc $ base m1
    print $ toDoc $ trans m1
