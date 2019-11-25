module Eval where

import           Parser                         ( LispVal(..) )

eval :: LispVal -> LispVal
eval val@(String _                             ) = val
eval val@(Number _                             ) = val
eval val@(Bool   _                             ) = val
eval (    List   [Atom "quote", val           ]) = val
eval (    List   [Atom "list?", List _        ]) = Bool True
eval (    List   [Atom "list?", DottedList _ _]) = Bool True
eval (    List   (Atom f : args)               ) = apply f $ map eval args

apply :: String -> [LispVal] -> LispVal
apply f args = maybe (Bool False) ($ args) $ lookup f primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives =
  [ ("+"        , numericBinOp (+))
  , ("-"        , numericBinOp (-))
  , ("*"        , numericBinOp (*))
  , ("/"        , numericBinOp div)
  , ("mod"      , numericBinOp mod)
  , ("quotient" , numericBinOp quot)
  , ("remainder", numericBinOp rem)
  , ("symbol?"  , unaryOp isSymbol)
  , ("string?"  , unaryOp isString)
  , ("number?"  , unaryOp isNumber)
  , ("bool?"    , unaryOp isBool)
  , ("list?"    , unaryOp isList)
  ]

numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinOp f params = Number $ foldl1 f $ map unpackNum params

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> LispVal
unaryOp f [n] = f n

isSymbol, isString, isNumber, isBool, isList :: LispVal -> LispVal
isSymbol (Atom _) = Bool True
isSymbol _        = Bool False
isString (String _) = Bool True
isString _          = Bool False
isNumber (Number _) = Bool True
isNumber _          = Bool False
isBool (Bool _) = Bool True
isBool _        = Bool False
isList (List _        ) = Bool True
isList (DottedList _ _) = Bool True
isList _                = Bool False

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) =
  let parsed = reads n :: [(Integer, String)]
  in  if null parsed then 0 else fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum _          = 0
