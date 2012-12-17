--
-- In decreasing order of (weight / length), doesn't matter how to break ties.
--

-- output: sum of weighted completion times
import System.IO
import System.Environment
import Control.Monad
import Control.Monad.Error
import Data.Heap

{-- 'guard' works in this case only if IO is a MonadPlus instance. It's defined
 - in Control.Monad.Error in mtl package.
 --} 
main :: IO ()
main = getArgs >>= \(f:_) ->
    readFile f >>= \s ->
    let count:xs = lines s
    in  guard (length xs == read count) >>
        let lst  = toAscList (toHeap xs)
            (_, sum) = complSum lst
        in  print sum

newtype WLprio = WLprio { getWLprio :: (Integer, Integer) } deriving (Eq)

instance Ord WLprio where
    -- compare :: WLprio -> WLprio -> Ordering
    compare (WLprio (w1, l1)) (WLprio (w2, l2)) =
        compare ((fromIntegral w1) / (fromIntegral l1))
                ((fromIntegral w2) / (fromIntegral l2))

type WLval = (Integer, Integer)

toHeap :: [String] -> MaxPrioHeap WLprio WLval
toHeap = foldl toHeap' empty
    where toHeap' h s
            | length (words s) /= 2 = h
            | otherwise             = let (w:l:_) = words s
                                          p       = (read w, read l)
                                      in  insert (WLprio p, p) h

complSum :: [(WLprio, WLval)] -> (Integer, Integer)
complSum = foldl complSum' (0, 0)
    where complSum' (cmp, sum) (_, (w, l)) =
            let len = cmp + l
            in  (len, sum + w * len)


printItems :: [(WLprio, WLval)] -> IO ()
printItems []              = return ()
printItems ((_, (w, l)):xs) =
    (print $ (show w) ++ ", " ++ (show l) ++ " => " ++ show (w - l)) >>
    printItems xs

