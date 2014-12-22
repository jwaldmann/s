import Tab.Exp
import Tab.Opt       

import qualified S.Table as ST
import qualified Data.Map as M      

main = do
    let m = ST.complete $ ST.trans
        n = 39
        target (x,y) = mod (m M.! (x,y)) n
    work n target 100 100
