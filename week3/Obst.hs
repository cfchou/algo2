
-- Optimal Binary Search Tree
import Data.Array
import Debug.Trace

os = [(1, 5), (2, 40), (3, 8), (4, 4), (5, 10), (6, 10), (7, 23)]

obst :: [(Int, Int)] -> (Int, Array (Int, Int) Int)
obst items = 
    let n = length items 
        ts = listArray (1, n) items
        arr = array ((1, 1), (n, n)) [ ((i, j), f i j)
                                     | i <- [1..n], j <- [1..n] ]
        f i j
            | i > j = 0
            | i == j = snd $ ts ! i
            | i + 1 == j = let cost = sum $ map (snd . (ts !)) [i..j]
                           in  cost + minimum [ arr ! (i, i), arr ! (j, j) ]
            | otherwise = let cost = sum $ map (snd . (ts !)) [i..j]
                          in  minimum [ arr ! (i, r - 1) + 
                                            arr ! (r + 1, j) +
                                            cost
                                          | r <- [i..j], r > i, r < j ]
    in  (arr ! (1, n), arr)

-- REPL would print the memoization array, therefore every entry will be
-- evaluated.
run1 = obst os


showOBST :: Array (Int, Int) Int -> String
showOBST arr = 
    let n = truncate . sqrt . fromIntegral . rangeSize . bounds $
            arr
        f i j
            | i > j = error "Error " ++ show i ++ " > " ++ show j
            | i == j = show i
            | i + 1 == j = if arr ! (i, i) < arr ! (j, j)
                           then "(" ++ show i ++ "," ++ show j ++ ",_)"
                           else "(_," ++ show i ++ "," ++ show j ++ ")"
            | otherwise = let rg = [(i + 1)..(j - 1)] 
                              zs = zip rg [ arr ! (i, r - 1) + arr ! (r + 1, j)
                                   | r <- rg ]
                              fmin (p, q) (p', q') = if q' < q then (p', q') 
                                                     else (p, q)
                              (r', _) = foldl fmin (i + 1, snd(head zs)) zs
                              lt = f i (r' - 1)
                              rt = f (r' + 1) j
                          in  "(" ++ lt ++ "," ++ show r' ++ "," ++ rt ++ ")"
    in  if n * n /= (fromIntegral . rangeSize . bounds) arr
        then error "Error invalid array size"  -- sanity check
        else f 1 n

run2 = showOBST . snd . obst $ os 


{--
obst items = 
    let n = length items 
        ts = listArray (1, n) items
        arr = array ((1, 1), (n, n)) [ ((i, j), f i j)
                                     | i <- [1..n], j <- [1..n] ]
        f i j
            | i > j = 0
            | i == j = let cost = snd $ ts ! i
                       in  trace (show i ++ "=" ++ show j ++ ", " ++ 
                               show cost) $ cost
            | i + 1 == j = let cost = sum $ map (snd . (ts !)) [i..j]
                           in  cost + minimum [ arr ! (i, i), arr ! (j, j) ]
            | otherwise = let cost = sum $ map (snd . (ts !)) [i..j]
                              k = minimum [ arr ! (i, r - 1) + 
                                            arr ! (r + 1, j) +
                                            cost
                                          | r <- [i..j], r > i, r < j ]
                          in  trace (show i ++ "," ++ show j ++ 
                                     ", " ++ show k) $ k
    in  (arr ! (1, n), arr)
--}
--
