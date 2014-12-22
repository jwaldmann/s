import Tab.Exp
import Tab.Bin (from)
import Tab.Opt       

import qualified S.Table as ST
import qualified Data.Map as M      

target = ST.complete $ ST.trans

main = do
    work target 100 100
