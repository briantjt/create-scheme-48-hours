{-# LANGUAGE ExistentialQuantification #-}
module Eval where

import           Parser                         ( LispVal(..)
                                                , LispError(..)
                                                , ThrowsError
                                                )
import           Control.Monad.Except
import           Text.ParserCombinators.Parsec

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals a b (AnyUnpacker unpacker) =
  do
      unpacked1 <- unpacker a
      unpacked2 <- unpacker b
      return $ unpacked1 == unpacked2
    `catchError` const (return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [List a, List b] = return $ Bool $ (length a == length b) && all
  eqvPair
  (zip a b)
 where
  eqvPair (x, y) = case equal [x, y] of
    Left  err        -> False
    Right (Bool val) -> val
equal [a, b] = do
  primitiveEquals <- or <$> mapM
    (unpackEquals a b)
    [AnyUnpacker unpackNum, AnyUnpacker unpackBool, AnyUnpacker unpackStr]
  eqvEquals <- eqv [a, b]
  return $ Bool (primitiveEquals || let (Bool x) = eqvEquals in x)

eval :: LispVal -> ThrowsError LispVal
eval val@(String _                             ) = return val
eval val@(Number _                             ) = return val
eval val@(Bool   _                             ) = return val
eval (    List   [Atom "quote", val           ]) = return val
eval (    List   [Atom "list?", List _        ]) = return $ Bool True
eval (    List   [Atom "list?", DottedList _ _]) = return $ Bool True
eval (    List   [Atom "list?", _             ]) = return $ Bool False
eval (    List   [Atom "if", pred, conseq, alt]) = evalCondition pred conseq alt
eval (    List   (Atom f : args)               ) = mapM eval args >>= apply f

apply :: String -> [LispVal] -> ThrowsError LispVal
apply f args = maybe
  (throwError $ NotFunction "Unrecognized primitive function args" f)
  ($ args)
  (lookup f primitives)

evalCondition :: LispVal -> LispVal -> LispVal -> ThrowsError LispVal
evalCondition pred conseq alt = do
  result <- eval pred
  case result of
    Bool True  -> eval conseq
    Bool False -> eval alt
    _          -> throwError $ TypeMismatch "bool" result

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
  [ ("+"             , numericBinOp (+))
  , ("-"             , numericBinOp (-))
  , ("*"             , numericBinOp (*))
  , ("/"             , numericBinOp div)
  , ("mod"           , numericBinOp mod)
  , ("quotient"      , numericBinOp quot)
  , ("remainder"     , numericBinOp rem)
  , ("="             , numBoolBinOp (==))
  , (">"             , numBoolBinOp (>))
  , (">="            , numBoolBinOp (>=))
  , ("<"             , numBoolBinOp (<))
  , ("<="            , numBoolBinOp (<=))
  , ("&&"            , boolBoolBinOp (&&))
  , ("||"            , boolBoolBinOp (||))
  , ("string=?"      , strBoolBinOp (==))
  , ("string>?"      , strBoolBinOp (>))
  , ("string>=?"     , strBoolBinOp (>=))
  , ("string<?"      , strBoolBinOp (<))
  , ("string<=?"     , strBoolBinOp (<=))
  , ("car"           , car)
  , ("cdr"           , cdr)
  , ("cons"          , cons)
  , ("eq?"           , eqv)
  , ("eqv?"          , eqv)
  , ("equal?"        , equal)
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

boolBinOp
  :: (LispVal -> ThrowsError a)
  -> (a -> a -> Bool)
  -> [LispVal]
  -> ThrowsError LispVal
boolBinOp unpacker op args = if length args /= 2
  then throwError $ NumArgs 2 args
  else do
    left  <- unpacker $ args !! 0
    right <- unpacker $ args !! 1
    return $ Bool (left `op` right)

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool   s) = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "bool" notBool

numBoolBinOp :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinOp = boolBinOp unpackNum

strBoolBinOp :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinOp = boolBinOp unpackStr

boolBoolBinOp :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinOp = boolBinOp unpackBool

car :: [LispVal] -> ThrowsError LispVal
car [List (x : _)        ] = return x
car [DottedList (x : _) _] = return x
car [badArg              ] = throwError $ TypeMismatch "pair" badArg
car badArgList             = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)         ] = return $ List xs
cdr [DottedList [_     ] xs] = return xs
cdr [DottedList (_ : xs) x ] = return $ DottedList xs x
cdr [badArg                ] = throwError $ TypeMismatch "pair" badArg
cdr badArgList               = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []            ] = return $ List [x1]
cons [x , List xs            ] = return $ List (x : xs)
cons [x , DottedList xs xlast] = return $ DottedList (x : xs) xlast

cons [x1, x2                 ] = return $ DottedList [x1] x2
cons badArgList                = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool   a, Bool b  ] = return $ Bool (a == b)
eqv [String a, String b] = return $ Bool (a == b)
eqv [Number a, Number b] = return $ Bool (a == b)
eqv [Atom   a, Atom b  ] = return $ Bool (a == b)
eqv [DottedList xs x, DottedList ys y] =
  eqv [List (xs ++ [x]), List (ys ++ [y])]
eqv [List xs, List ys] = return $ Bool $ (length xs == length ys) && all
  eqvPair
  (zip xs ys)
 where
  eqvPair (x, y) = case eqv [x, y] of
    Left  err        -> False
    Right (Bool val) -> val
eqv [_, _]     = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList
