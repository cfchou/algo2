--
-- 
--

-- output: sum of weighted completion times
import System.IO
import System.Environment
import Control.Monad
import Control.Monad.Error
import Data.Set (Set, insert, delete, partition, findMin, empty, size, member
                , singleton)
import qualified Data.Set as S (foldr)
import Debug.Trace

main :: IO ()
main = getArgs >>= \(f:_) ->
    readFile f >>= \s ->
    let _:xs = lines s
        es = mst $ toVESet xs
        c = mstCost es
    in  print c

mstCost :: Set Edge -> Int 
mstCost = S.foldr mstCost' 0
    where mstCost' (Edge _ _ c) a = c + a

toVESet :: [String] -> Graph
toVESet ss = trace ("total: " ++ show (length ss)) $ foldl toVESet' (empty, empty) ss
    where toVESet' (vs, es) s =
            let (v1:v2:c:_) = words s
            --in  trace (show(size vs) ++ " " ++ v1 ++ " " ++ v2 ++ " $ " ++ show (size es) ++ " " ++ c) $
            in  trace (show (size es) ++ " " ++ v1 ++ " " ++ v2 ++ " " ++ c) $
            --in  trace (v1 ++ " " ++ v2 ++ " " ++ c) $
                    (insert (read v2) (insert (read v1) vs),
                     insert (Edge (read v1) (read v2) (read c)) es)

type Cost = Int
type Vertex = Int
type Graph = (Set Vertex, Set Edge)
data Edge = Edge Vertex Vertex Cost deriving (Eq)

instance Ord Edge where
    compare (Edge _ _ c1) (Edge _ _ c2) = compare c1 c2


mst :: Graph -> Set Edge
mst (vs, es) = let e@(Edge v _ _) =
                    --trace (show(size vs) ++ ", " ++ show (size es)) $
                        findMin es
               in  findMST (singleton v) (delete v vs) empty es


findMST :: Set Vertex -> Set Vertex -> Set Edge -> Set Edge -> Set Edge
findMST vMst vRest eMst eRest 
    | size vRest == 0 = eMst
    | otherwise = 
        let (eCross, _) = partition crossE eRest
            e@(Edge _ v _) = findMin' eCross
        in  findMST (insert v vMst) (delete v vRest) (insert e eMst) 
                    (delete e eRest)
        where crossE (Edge v1 v2 c) = (member v1 vMst) && (member v2 vRest)
              findMin' = findMin
              --findMin' = trace ("es2: " ++ show (size vMst) ++ ", " ++ 
              --                  show (size vRest)) findMin

