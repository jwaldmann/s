import S.Type
import S.Model
import S.ToDoc
import S.Table
import qualified S.TableHead as TH
import qualified S.Normal
import qualified S.Head
import S.Verify

import Control.Monad ( forM_ )
import Data.Maybe (isNothing)
import System.IO 

main = do
    -- print_labelled
    -- write_head_table
    check_head_normalization

print_labelled = do
    local S.Table.trans

check_normalization = do
    forM_ (concat terms) $ \ t -> do
        let v = value t
            n = S.Normal.normal 100 t
        if isNothing v == isNothing n then putStr "." else error $ show $ toDoc (t,v,n)
        hFlush stdout

check_head_normalization = do
    forM_ (concat terms) $ \ t -> do
        let v = TH.normalizing t
            n = null $ drop 100 $ S.Head.plain t
        if v == n then if v then putStr "." else putStr "!" else error $ show $ toDoc (t,v,n)
        hFlush stdout

write_head_table = do
    m0 <- model0 8 200
    m1 <- build_head m0
    print $ toDoc $ base m1
    print $ toDoc $ S.Model.trans m1
    print $ toDoc $ S.Model.accept m1
