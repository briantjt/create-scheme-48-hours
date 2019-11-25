module Main where

import           System.Environment
import           Parser                         ( readExpr )
import Eval

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
