module Main where

import Eval
import Parser
import qualified Data.Map.Strict as Map
import Text.Megaparsec
import System.Environment
import Data.Either

main :: IO ()
main = do
  args <- getArgs
  stdlib <- readFile "./src/stdlib.lama"
  program <- readFile $ head args
  case parse expr "" (stdlib ++ program) of
    Right e -> case eval e Map.empty of
      Left err -> print("Runtime Error: " ++ err)
      Right x -> case x of
        DLambda _ _ _ -> print("<Lambda>")
        _ -> print(x)
    Left err -> print("Parse Error: " ++ show(err))
