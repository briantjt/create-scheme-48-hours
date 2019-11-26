module Eval where

import           Parser                         ( LispVal(..)
                                                , LispError(..)
                                                , ThrowsError
                                                )
import           Control.Monad.Except
import           Text.ParserCombinators.Parsec

eval :: LispVal -> ThrowsError LispVal
eval val@(String _                             ) = return val
eval val@(Number _                             ) = return val
eval val@(Bool   _                             ) = return val
eval (    List   [Atom "quote", val           ]) = return val
eval (    List   [Atom "list?", List _        ]) = return $ Bool True
eval (    List   [Atom "list?", DottedList _ _]) = return $ Bool True
eval (    List   [Atom "list?", _             ]) = return $ Bool False
eval (    List   (Atom f : args)               ) = mapM eval args >>= apply f

apply :: String -> [LispVal] -> ThrowsError LispVal
apply f args = maybe
  (throwError $ NotFunction "Unrecognized primitive function args" f)
  ($ args)
  (lookup f primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
  [ ("+"             , numericBinOp (+))
  , ("-"             , numericBinOp (-))
  , ("*"             , numericBinOp (*))
  , ("/"             , numericBinOp div)
  , ("mod"           , numericBinOp mod)
  , ("quotient"      , numericBinOp quot)
  , ("remainder"     , numericBinOp rem)
  , ("symbol?"       , unaryOp isSymbol)
  , ("string?"       , unaryOp isString)
  , ("number?"       , unaryOp isNumber)
  , ("bool?"         , unaryOp isBool)
  , ("symbol->string", unaryOp symbol2String)
  , ("string->symbol", unaryOp string2Symbol)
  ]

numericBinOp
  :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinOp f [] = throwError $ NumArgs 2 []
numericBinOp f singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinOp f params = mapM unpackNum params >>= return . Number . foldl1 f

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp _ []   = throwError $ NumArgs 1 []
unaryOp f [n]  = return $ f n
unaryOp _ args = throwError $ NumArgs 1 args

-- Unary Operations
isSymbol, isString, isNumber, isBool :: LispVal -> LispVal
isSymbol (Atom _) = Bool True
isSymbol _        = Bool False
isString (String _) = Bool True
isString _          = Bool False
isNumber (Number _) = Bool True
isNumber _          = Bool False
isBool (Bool _) = Bool True
isBool _        = Bool False
symbol2String (Atom s) = String s
symbol2String _        = String ""
string2Symbol (String s) = Atom s
string2Symbol _          = Atom ""

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) =
  let parsed = reads n :: [(Integer, String)]
  in  if null parsed
        then throwError $ TypeMismatch "number" $ String n
        else return $ fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNumber  = throwError $ TypeMismatch "number" notNumber
