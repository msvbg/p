#!/usr/local/bin/runghc
import Prelude hiding (putStrLn, map)
import Data.ByteString (map)
import Data.ByteString.Char8 (putStrLn)
import System.Entropy

main = getEntropy 20 >>= putStrLn . (map encode)

encode s | s < (126 - 33) = 33 + s
         | otherwise = encode $ s `mod` (126 - 33)
