--
--
import System.IO
import System.Environment
import Control.Monad
import Control.Monad.Error
import qualified Data.Heap as H
--import qualified Data.Set as S
import qualified Data.Map as M
import Data.Array.IArray

import Control.Monad.ST
import Data.Array.ST

import Debug.Trace

main :: IO ()
main = getArgs >>= \arg ->
       if 2 > (length arg) then
           print "Cluster k file" 
       else let (k:f:_) = arg
            in  readFile f >>= \s ->
                let (count:xs) = lines s
                    cnt = read count
                    h = initEdges xs
                    cs = initClusters cnt
                    lst = initLeaders cnt
                    kc = runST (lst >>= \arr -> kmerge arr (Klustering h cs) 
                                (read k))
                in  case H.view (getHeap kc) of
                        Nothing -> print "fail"
                        Just (Edge _ _ c, _) -> print (show c)

type Cost = Int
type Leader = Int
type Vertex = Int
type Cluster = [Vertex]
data Edge = Edge Vertex Vertex Cost deriving (Eq, Show)
type LM = M.Map Leader Cluster
data Klustering = Klustering { getHeap :: H.MinHeap Edge
                             , getClusters :: LM
                             }
instance Ord Edge where
    compare (Edge _ _ c1) (Edge _ _ c2) = compare c1 c2

initEdges :: [String] -> H.MinHeap Edge
initEdges = foldr f H.empty
    where f e h = let (u:v:c:_) = (words e)
                  in  H.insert (Edge (read u) (read v) (read c)) h

type LA s = STArray s Vertex Leader 
initLeaders :: Int -> ST s (LA s)
{--
initLeaders n =
    let arr = array (1, n) [(i, i) | i <- [1..n]] :: Array Vertex Leader
    in  thaw arr
--}
initLeaders n = newArray (1, n) 0 >>= \arr ->
                forM_ [1 .. n] (\i -> writeArray arr i i) >>
                return arr

initClusters :: Int -> M.Map Leader Cluster
initClusters n = foldr f M.empty [1..n]
    --where f i m = M.insert i [(i, i)] m
    where f i m = M.insert i [i] m

find :: LA s -> Vertex -> ST s Leader
find = readArray

union :: LA s -> LM -> Vertex -> Vertex -> ST s LM
union arr mp u v = find arr u >>= \lu ->
                   find arr v >>= \lv ->
                   if lu == lv then return mp
                   else let (Just uc) = M.lookup lu mp
                            (Just vc) = M.lookup lv mp
                            uvc = uc ++ vc
                            mp' = M.delete lv (M.delete lu mp)
                        in  if (length uc) < (length vc) 
                            then unionTo uc lv >>
                                 (return $ M.insert lv uvc mp')
                            else unionTo vc lu >>
                                 (return $ M.insert lu uvc mp')
    where unionTo cs l = forM_ cs $ \i -> writeArray arr i l

kmerge :: LA s -> Klustering -> Int -> ST s Klustering
kmerge arr kc k =
    case H.view (getHeap kc) of
        Nothing -> trace "Nothing return kc" $ return kc
        Just (Edge u v c, h) ->
            find arr u >>= \lu ->
            find arr v >>= \lv ->
            let cs = getClusters kc
            in  trace ("pop " ++ show u ++ " " ++ show v ++ " " ++ show c) $
                    if lu == lv then kmerge arr (Klustering h cs) k
                    else if M.size cs == k then return kc
                            else union arr cs lu lv >>= \cs' ->
                                kmerge arr (Klustering h cs') k

