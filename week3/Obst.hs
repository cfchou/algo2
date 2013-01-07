
-- Optimal Binary Search Tree
import Data.Array
import Debug.Trace

os = [(1, 5), (2, 40), (3, 8), (4, 4), (5, 10), (6, 10), (7, 23)]

obst :: [(Int, Int)] -> Int
obst items = 
    let n = length items 
        ts = listArray (1, n) items
        arr = array ((1, 1), (n, n)) [ ((i, j), f i j)
                                     | i <- [1..n], j <- [1..n] ]
        f i j
            | i == j = trace (show i ++ ", " ++ show j) $ snd $ ts ! i
            | otherwise = let cost = sum $ map (snd . (ts !)) [i..j]
                              lst = [ arr ! (i, r - 1) + arr ! (r + 1, j) +
                                      cost
                                    | r <- [i..j], r > i, r < j ]
                          in  if null lst
                              then trace ((show i) ++ "," ++ (show j) ++ 
                                   ", " ++ (show cost)) $ cost
                              else let k = minimum lst
                                   in  trace ((show i) ++ "," ++ (show j) ++ 
                                   ", " ++ (show k)) $ k
    in  arr ! (1, n)

run = obst os

