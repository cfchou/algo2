
{-# LANGUAGE TupleSections #-}

import System.IO
import System.Environment
import Control.Monad
import Control.Applicative
import Data.Bits
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.ST
import Data.Array.ST
import Data.Array.IArray
import Data.List

import Test.QuickCheck

-- import Debug.Trace
trace :: a -> b -> b
trace _ = id

traceShow :: a -> b -> b
traceShow = trace



type Leader = Int
type Visited = Bool
data Dots s = Dots { pmap :: M.Map String [Int] -- pattern map
                   , larr :: STArray s Int (Visited, Leader) -- leader array
                   }

--mkDots :: [String] -> ST s (STArray s Int (Visited, Leader))
mkDots :: [String] -> ST s (Dots s)
mkDots ss = let pm = foldr addm M.empty $ zip [1..] ss
                sz = length ss
            in  newArray (1, sz) (False, 0) >>= \arr ->
                mapM_ (\i -> writeArray arr i (False, i)) [1..sz] >>
                return (Dots pm arr)
    where addm (i, p) pm' = M.insertWith (++) p [i] pm'

main :: IO ()
main = getArgs >>= \args ->
       if length args < 1 then putStrLn "Error"
       else lines <$> readFile (head args) >>= \xs ->
            if length xs < 1 then putStrLn "Error"
            else let ss = map (filter (/= ' ')) (tail xs)
                     len = runST (goDfs (zip [1..] ss) (mkDots ss)) 
                     --len = runST (test2 (mkDots ss))
                 in  putStrLn $ show len


goDfs :: [(Int, String)] -> ST s (Dots s) -> ST s Int
goDfs xs std =
    foldr (\n@(i, _) std' -> dfs i n std') std xs >>= \dots ->
    mapM (readArray (larr dots)) [1..length xs] >>= \ldrs ->
    let lst' = zip [1..length xs] ldrs
        lst = traceShow lst' $ lst' -- :: [(Int, (Visited, Leader))]
    in  return $ numLeader lst

numLeader :: [(Int, (Visited, Leader))] -> Int
numLeader = S.size . (foldr (\(_, (_, ldr)) s -> S.insert ldr s) S.empty)



{--
sequence :: [m b] -> m [b]
mapM :: (a -> m b) -> [a] -> m [b]
mapM f = sequence . map f 
--}


-- Prefix DFS
dfs :: Int -> (Int, String) -> ST s (Dots s) -> ST s (Dots s)
dfs leader (i, s) std =
    std >>= \dots ->
    let f p = zip (maybe [] id (M.lookup p (pmap dots))) $ repeat p
        nodes' = concatMap f $ s:(flipAll s)
        --nodes = trace ("n: " ++ show (length nodes')) $ nodes'
        nodes = traceShow nodes' nodes'
        ar = larr dots
        g a i e = trace "++" $ writeArray a i e
        h n std' = trace "f" $ dfs leader n std'
    in  trace ("=====" ++ show i ++ ", l: " ++ show leader) $ 
            readArray ar i >>= \(bv, lv) ->
        if True == bv then trace "--" $ return dots
        else g ar i (True, leader) >>
             foldr h (return dots) nodes
        {--
        else writeArray ar i (True, leader) >>
             foldr (\n std' -> dfs leader n std') (return dots) nodes
             --}

flipAll :: String -> [String]
flipAll = (++) <$> flip1 <*> flip2
{--
flipAll s = let ret = fa s
            in  traceShow (length ret) $ ret
    where fa s = ((++) <$> flip1 <*> flip2) s
--}
flip1 s = map f $ zip [0..] s
    where f (i, c) = let (xs, ys) = splitAt i s
                     in  if c == '0' then xs ++ '1':(tail ys)
                         else xs ++ '0':(tail ys)

flip2 :: String -> [String]
flip2 s = concatMap (flip2' s) [0..(length s) - 1]
    where flip2' s i =
              let (xs, ys) = splitAt i s
                  yss = flip1 (tail ys)
                  op1 = map (\s' -> xs ++ '1':s') yss
                  op2 = map (\s' -> xs ++ '0':s') yss
              in  if s !! i == '0' then op1
                  else op2



test xs = foldl1 (\a i -> a + test' i) xs
    where test' i' = trace "run" $ i'

