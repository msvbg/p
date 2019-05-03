module Util
    ( rstrip
    )
where

-- Shamelessly stolen from Data.String.Utils. That dependency was a little bit
-- heavy for just one function

wschars :: String
wschars = " \t\r\n"

lstrip :: String -> String
lstrip s = case s of
    []       -> []
    (x : xs) -> if elem x wschars then lstrip xs else s

rstrip :: String -> String
rstrip = reverse . lstrip . reverse
