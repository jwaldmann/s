{-# language GeneralizedNewtypeDeriving #-}

module S.DAG where

import S.Type 

import qualified Data.GraphViz as V
import qualified Data.GraphViz.Attributes.Complete as V
import  Data.GraphViz.Attributes.Colors.X11

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Control.Monad.State.Strict as T
import Control.Applicative ((<$>),(<*>))
import Control.Monad ( guard )

newtype Ptr = Ptr Int
    deriving ( Eq, Ord, Show, V.PrintDot )

data Node = Leaf | Branch Ptr Ptr
    deriving ( Eq, Ord, Show )

data St = St { norm :: ! (M.Map Ptr Ptr)
             , fore :: ! (M.Map Ptr Node)
             , back :: ! (M.Map Node Ptr)
             }
    deriving Show

dag t = flip T.execState empty $ no t


normal t = flip T.evalState empty $ do
    p <- no t
    o <- M.size <$> T.gets fore
    r <- reachable p
    -- e <- expand p
    return (M.size r, o)

reachable p = do
    f <- T.gets fore
    let next p = case f M.! p of 
            Leaf -> [] ; Branch l r -> [l,r] 
        r = hull p next
        m = M.fromSet (const ()) $ S.fromList r
    return $ M.intersection f m

hull x0 f = 
    let handle done todo = case S.minView todo of
            Nothing -> []
            Just (t, odo) -> 
                if S.member t done
                then handle done odo
                else t : handle (S.insert t done)
                         (S.union (S.fromList $ f t) odo)
    in  handle S.empty $ S.singleton x0

empty = St { norm = M.empty, fore=M.empty,back=M.empty }

fresh = do
    st <- T.get
    return $ Ptr $ M.size $ fore st

expand p = do
    st <- T.get
    case fore st M.! p of
        Leaf -> return s
        Branch l r -> app <$> expand l <*> expand r

node :: Node -> T.State St Ptr
node n = do
    st <- T.get
    case M.lookup n $ back st of
        Just p -> return p
        Nothing -> do
            p <- fresh
            T.put $ st 
                    { fore = M.insert p n $ fore st 
                    , back = M.insert n p $ back st 
                    }
            return p

no :: T -> T.State St Ptr
no t = case t of
    S -> node Leaf
    App {fun=f,arg=a} -> do
        bf <- no f ; ba <- no a ; apply bf ba

-- * hash consing computation of normal form
-- (assuming it exists, need to check this elsewhere)

apply l r = do
    p <- node $ Branch l r
    st <- T.get 
    case M.lookup p $ norm st of      
         Just q -> return q
         Nothing -> do
            q <- match p (return p) $ \ sxy z -> 
                 match sxy (return p) $ \ sx y ->
                 match sx (return p) $ \ s x -> 
                 match s (do 
                     xz <- apply x z
                     yz <- apply y z
                     apply xz yz
                   ) undefined
            T.modify $ \ st -> 
                st { norm = M.insert p q $ norm st }
            return q

match p leaf branch = do
    st <- T.get
    case fore st M.! p of
        Leaf -> leaf
        Branch l r -> branch l r

-------------------------------------------------------------------------

display t = V.runGraphvizCanvas V.Dot (viz $ dag t) V.Gtk

-- viz :: M.Map Ptr Node -> V.DotGraph Ptr
viz st = 
    let tochildren = do
              (from, Branch l r) <- M.toList $ fore st
              (to, col) <- [(l,Blue),(r,Red)]
              return $ V.DotEdge from to [ V.Color [V.WC (V.X11Color col) Nothing ] ]
        tonormal = do
              (from, to) <- M.toList $ norm st
              guard $ from /= to
              return $ V.DotEdge from to [ V.Color [V.WC (V.X11Color Green) Nothing ] ]
    in  V.DotGraph
    { V.strictGraph = False
    , V.directedGraph = True
    , V.graphID = Nothing
    , V.graphStatements = V.DotStmts
        { V.attrStmts = []
        , V.subGraphs = []
        , V.nodeStmts = do
              (from, _) <- M.toList $ fore st
              return $ V.DotNode from [ V.Shape V.PlainText ]
        , V.edgeStmts = tochildren ++ tonormal
        }
    }
        
        
        
