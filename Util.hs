module Util
  ( fAnd
  , fOr
  , tuplify2
  , dump
  , output
  , if'
  ) where

import System.IO
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

dump :: Show a => a -> a 
dump a = output a a

output :: Show a => a -> b -> b
output a = seq (unsafePerformIO $ hPutStrLn stderr $ show a)
