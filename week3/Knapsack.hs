
import Data.Array

os = [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4)]

-- unbounded knapsack
ub_knapsack items wmax = 
    let arr = array (1, wmax) [ (w, f w) | w <- [1..wmax] ]
        f 0 = 0
        f w = maximum $ 0:[arr ! (w - wi) + vi | (vi, wi) <- items, wi < w] 
    in  arr ! wmax

run1 = ub_knapsack os 15


-- 1-0 knapsack
knapsack :: [(Int, Int)] -> Int -> Int
knapsack items wmax = kp $ listArray (1, length items) items
    where kp items' = 
            let n = rangeSize (bounds items')
                arr = array ((0, 0), (n, wmax)) [ ((i, w), f i w) |
                          i <- [0..n], w <- [0..wmax] ]
                f 0 _ = 0
                f _ 0 = 0
                f i w = let (vi, wi) = items' ! i
                        in  if w < wi then arr ! (i - 1, w)
                            else max (arr ! (i - 1, w - wi) + vi) 
                                     (arr ! (i - 1, w))
            in  arr ! (n, wmax)

run2 = knapsack os 15

