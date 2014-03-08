import S.Type
import S.Model
import S.ToDoc
import S.Table
import qualified S.Normal
import S.Verify

import Control.Monad ( forM_ )
import Data.Maybe (isNothing)
import System.IO 

main = do
    -- print_labelled
    write_head_table

print_labelled = do
    local S.Table.trans

check_normalization = do
    forM_ (concat terms) $ \ t -> do
        let v = value t
            n = S.Normal.normal 100 t
        if isNothing v == isNothing n then putStr "." else error $ show $ toDoc (t,v,n)
        hFlush stdout

write_head_table = do
    m0 <- model0 7 100
    m1 <- build_head m0
    print $ toDoc $ base m1
    print $ toDoc $ S.Model.trans m1
    print $ toDoc $ S.Model.accept m1
