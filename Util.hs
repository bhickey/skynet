module Util
  ( fAnd
  , fOr
  , tuplify2
  , output
  , if'
  ) where

import System.IO.Unsafe
 
fAnd :: a -> [a -> Bool] -> Bool
fAnd x = all ($x)

fOr :: a -> [a -> Bool] -> Bool
fOr x = any ($x)

tuplify2 :: [a] -> (a,a)
tuplify2 [x,y] = (x,y)
tuplify2 _ = error "tuplify2"

if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ y = y

output :: String -> b -> b
output a = seq (unsafePerformIO $ appendFile "dump.out" ("TURN\n" ++ a))
