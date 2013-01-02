--
-- Prim's algorithm (preliminary) for MST
--

-- output: sum of weighted completion times
import System.IO
import System.Environment
import Data.Monoid (mappend)
import Control.Monad
import Control.Monad.Error
import Data.Set (Set, insert, delete, partition, findMin, empty, size, member
                , singleton, toList)
import qualified Data.Set as S (foldr, foldl)
import Data.List (minimumBy)
import Debug.Trace

type Cost = Int
type Vertex = Int
type Graph = (Set Vertex, Set Edge)
-- data Edge = Edge Vertex Vertex Cost deriving (Eq)
data Edge = Edge Vertex Vertex Cost

-- Note that (Set a) makes use of (Ord a) to spot duplicated items, not (Eq a).
-- So implementing (Eq Edge) or deriving Eq doesn't make much sense. 

instance Ord Edge where
    compare (Edge v1 u1 c1) (Edge v2 u2 c2) = compare c1 c2 `mappend`
                                              compare v1 v2 `mappend`
                                              compare u1 u2

main :: IO ()
main = getArgs >>= \(f:_) ->
    readFile f >>= \s ->
    let _:xs = lines s
        es = mst $ toGraph xs
        c = mstCost es
    in  print c

mstCost :: Set Edge -> Integer 
mstCost = S.foldr mstCost' 0
    where mstCost' (Edge _ _ c) a = (toInteger c) + a

toGraph :: [String] -> Graph
toGraph ss = trace ("total: " ++ show (length ss)) $
                 foldl toGraph' (empty, empty) ss
    where toGraph' (vs, es) s =
            let (v1:v2:c:_) = words s
                e = Edge (read v1) (read v2) (read c)
            in  (insert (read v2) (insert (read v1) vs), insert e es)


mst :: Graph -> Set Edge
mst (vs, es) = let e@(Edge v _ _) = findMin es -- actually doesn't matter if 
                                               -- it's minimal or not
               in  findMST (singleton v) (delete v vs) empty es


findMST :: Set Vertex -> Set Vertex -> Set Edge -> Set Edge -> Set Edge
findMST vMst vRest eMst eRest 
    | size vRest == 0 = eMst
    | otherwise = 
        let (eCross, _) = partition cross_edges eRest
            e@(Edge v u _) = min_edge $ toList eCross
            u' | (member u vRest) = u  -- vertex will get sucked in
               | otherwise = v
        in  findMST (insert u' vMst) (delete u' vRest) (insert e eMst)
                    (delete e eRest)
        where cross_edges (Edge v u c) = (member v vMst && member u vRest) ||
                                         (member u vMst && member v vRest)
              min_edge = minimumBy (\(Edge _ _ c1) (Edge _ _ c2) ->
                                    compare c1 c2)

