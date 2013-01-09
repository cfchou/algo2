-- Knapsack.hs
-- 
--
import System.IO
import System.Environment
import Control.Monad
import Control.Applicative

import Data.Array

main :: IO ()
main = getArgs >>= \args ->
       if length args < 1 then putStrLn "Error: filename?"
       else lines <$> readFile (head args) >>= \xs ->
            if length xs < 1 
            then putStrLn "Error: wrong format"
            else let cap = (read . head . words . head) xs :: Int
                     f ln = let ws = read . ((words ln) !!)
                            in  (ws 0, ws 1)
                     lst = map f $ tail xs
                 in  putStrLn $ show  (kpsk_list_v lst cap)
            {--
                     (v, s) = kpsk_list lst cap
                 in  putStrLn (show cap) >>
                     putStrLn (show  v) >>
                     putStrLn s
            --}

-- formula:
-- arr i w = maximum [ arr (i - 1) (w - warr!i) + varr!i,
--                     arr (i - 1) w ]
type Weight = Int
type Value = Int
kpsk :: Array Int (Value, Weight) -> Weight -> (Value, Array (Int, Int) Int)
kpsk items cap =
    let n = rangeSize . bounds $ items
        arr = array ((0, 0), (n, cap)) [ ((i, w), f i w) 
                                       | i <- [0..n], w <- [0..cap] ]
        f 0 w = 0
        f _ 0 = 0
        f i w = let (v', w') = items ! i
                in  if w - w' < 0 then arr ! (i - 1, w)
                    else maximum [ arr ! (i - 1, w - w') + v',
                                   arr ! (i - 1, w) ]
    in  (arr ! (n, cap), arr)

showKpsk :: Array (Int, Int) Int -> Array Int (Value, Weight) -> String
showKpsk arr items = 
    let (_, (n, cap)) = bounds $ arr
        f 0 w = ""
        f _ 0 = ""
        f i w = let (v', w') = items ! i
                in  if arr ! (i, w) == arr ! (i - 1, w) 
                    then f (i - 1) w
                    else "(" ++ show v' ++ ", " ++ show w' ++ ") " ++
                         show (w - w') ++ "\n" ++ (f (i - 1) (w - w'))
    in  f n cap
        
kpsk_list_v :: [(Value, Weight)] -> Weight -> Value
kpsk_list_v list cap =
    let items = listArray (1, length list) list
    in  fst $ kpsk items cap 

kpsk_list :: [(Value, Weight)] -> Weight -> (Value, String)
kpsk_list list cap =
    let items = listArray (1, length list) list
        (v, arr) = kpsk items cap
        s = showKpsk arr items 
    in  (v, s)

run = kpsk_list [(9, 9), (4, 12), (2, 2), (2, 1), (1, 1), (10, 4)] 15
