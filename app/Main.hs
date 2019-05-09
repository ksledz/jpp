module Main where

import Eval

main :: IO ()
main = let
  DInt n = test1
  in print n
