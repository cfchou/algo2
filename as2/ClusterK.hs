
import System.IO
import System.Environment
import Control.Monad
import Control.Applicative
import Data.Bits
import Data.Char

Map String [Int] -- Map bits [index]

main :: IO ()
main = getArgs >>= \args ->
       if length args < 1 then putStrLn "Error"
       else lines <$> readFile (head args) >>= \xs ->
            if length xs < 1 then putStrLn "Error"
            else let vs = zip [1..] $ map ((toInt . (filter (/= ' ')))) 
                          (tail xs)
                     -- vs' = [ f i j | i <- vs, j <- vs, fst i < fst j ]
                 in  print $ show (last vs)
    where z = ord '0'
          toInt = foldl (\n c -> n `shiftL` 1 + ord c - z) 0
          -- f (u, ui) (v, vi) = (u, v, popCount (ui `xor` vi)) 

flip1 :: String -> [String]
flip1 s = let sz = length s 
              arr = listArray (1, sz) s
          in  map f [1..length s]
    where f i =
        
