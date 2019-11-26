module Main where

import           System.Environment
import           Parser
import           Control.Monad
import           Eval

main :: IO ()
main = do
  args   <- getArgs
  let evaled = fmap show $ readExpr (head args) >>= eval
  putStrLn $ extractValue $ trapError evaled
