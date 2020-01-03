module Eval where

import           Parser                         ( trapError
                                                , extractValue
                                                , readExpr
                                                , readExprList
                                                )
import           Text.ParserCombinators.Parsec
import           Data.IORef
import           Data.Maybe
import           Control.Monad.Except
import           LispTypes
import           System.IO

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals a b (AnyUnpacker unpacker) =
  do
      unpacked1 <- unpacker a
      unpacked2 <- unpacker b
      return $ unpacked1 == unpacked2
    `catchError` const (return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [DottedList xs x, DottedList ys y] =
  equal [List (xs ++ [x]), List (ys ++ [y])]
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

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _                               ) = return val
eval env val@(Number _                               ) = return val
eval env val@(Bool   _                               ) = return val
eval env (    Atom   identifier                      ) = getVar env identifier
eval env (    List   [Atom "quote", val           ]  ) = return val
eval env (    List   [Atom "list?", List _        ]  ) = return $ Bool True
eval env (    List   [Atom "list?", DottedList _ _]  ) = return $ Bool True
eval env (    List   [Atom "list?", _             ]  ) = return $ Bool False
eval env (    List   (Atom "cond"       : conditions)) = cond env conditions
eval env (List (Atom "case" : key : clauses)) = evalCase env key clauses
eval env (List [Atom "if", pred, conseq, alt]) =
  evalCondition env pred conseq alt
eval env (List [Atom "set!", Atom var, form]) =
  eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
  eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
  makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body))
  = makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
  makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
  makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
  makeVarArgs varargs env [] body
eval env (List [Atom "load", String filename]) =
  load filename >>= liftM last . mapM (eval env)
eval env (List (function : args)) = do
  func    <- eval env function
  argVals <- mapM (eval env) args
  apply func argVals

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc f) args = liftThrows $ f args
apply (Func params varargs body closure) args =
  if num params /= num args && isNothing varargs
    then throwError $ NumArgs (num params) args
    else
      (liftIO $ bindVars closure $ zip params args)
      >>= bindVarArgs varargs
      >>= evalBody
 where
  remainingArgs = drop (length params) args
  num           = toInteger . length
  evalBody env = liftM last $ mapM (eval env) body
  bindVarArgs arg env = case arg of
    Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
    Nothing      -> return env
apply (IOFunc func) args = func args

primitiveBindings :: IO Env
primitiveBindings =
  nullEnv
    >>= (  flip bindVars
        $  map (makeFunc IOFunc)        ioPrimitives
        ++ map (makeFunc PrimitiveFunc) primitives
        )
  where makeFunc constructor (var, func) = (var, constructor func)

evalCondition :: Env -> LispVal -> LispVal -> LispVal -> IOThrowsError LispVal
evalCondition env pred conseq alt = do
  result <- eval env pred
  case result of
    Bool True  -> eval env conseq
    Bool False -> eval env alt
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
  , ("string-length" , stringLen)
  , ("string-ref"    , stringRef)
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

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives =
  [ ("apply"            , applyProc)
  , ("open-input-file"  , makePort ReadMode)
  , ("open-output-file" , makePort WriteMode)
  , ("close-input-port" , closePort)
  , ("close-output-port", closePort)
  , ("read"             , readProc)
  , ("write"            , writeProc)
  , ("read-contents"    , readContents)
  , ("read-all"         , readAll)
  ]

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args)     = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _           = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc []          = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj]            = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = String <$> (liftIO $ readFile filename)

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = List <$> (load filename)

numericBinOp
  :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinOp f []            = throwError $ NumArgs 2 []
numericBinOp f singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinOp f params        = Number . foldl1 f <$> mapM unpackNum params

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
    left  <- unpacker $ head args
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

cond :: Env -> [LispVal] -> IOThrowsError LispVal
cond env [List [Atom "else", value]     ] = eval env value
cond env (List [condition, value] : alts) = do
  result     <- eval env condition
  boolResult <- liftThrows $ unpackBool result
  if boolResult then eval env value else cond env alts
cond env [List a] = throwError $ NumArgs 2 a
cond env (a : _ ) = throwError $ NumArgs 2 [a]
cond env _        = throwError $ Default "Not viable alternative in cond"

evalCase :: Env -> LispVal -> [LispVal] -> IOThrowsError LispVal
evalCase env _ [] =
  throwError $ BadSpecialForm "no true clause in case expression: " (List [])
evalCase env key clauses = case head clauses of
  List (Atom "else" : exprs) -> last <$> mapM (eval env) exprs
  List (List datums : exprs) -> do
    result   <- eval env key
    equality <- liftThrows $ mapM (\x -> eqv [x, result]) datums
    if Bool True `elem` equality
      then last <$> mapM (eval env) exprs
      else evalCase env key (tail clauses)
  _ -> throwError $ BadSpecialForm "ill-formed case expression" (List clauses)

stringLen :: [LispVal] -> ThrowsError LispVal
stringLen [String s ] = return $ Number $ fromIntegral $ length s
stringLen [notString] = throwError $ TypeMismatch "not string" notString
stringLen badArgList  = throwError $ NumArgs 1 badArgList

stringRef :: [LispVal] -> ThrowsError LispVal
stringRef [String s, Number k]
  | length s < k' + 1 = throwError $ Default "Index out of bounds error"
  | otherwise         = return $ String [s !! k']
  where k' = fromIntegral k
stringRef [String s, notNum] = throwError $ TypeMismatch "not a number" notNum
stringRef [notString, _] = throwError $ TypeMismatch "not a string" notString
stringRef badArgList = throwError $ NumArgs 2 badArgList

nullEnv :: IO Env
nullEnv = newIORef []

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left  err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = extractValue <$> runExceptT (trapError action)

isBound :: Env -> String -> IO Bool
isBound envRef var = isJust . lookup var <$> readIORef envRef

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Getting an unbound variable" var)
        (liftIO . readIORef)
        (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Setting an unbound variable" var)
        (liftIO . (`writeIORef` value))
        (lookup var env)
  return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then setVar envRef var value >> return value
    else liftIO $ do
      valueRef <- newIORef value
      env      <- readIORef envRef
      writeIORef envRef ((var, valueRef) : env)
      return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
 where
  extendEnv bindings env = (++ env) <$> mapM addBinding bindings
  addBinding (var, value) = do
    ref <- newIORef value
    return (var, ref)

makeFunc varargs env params body =
  (return $ Func (map showVal params) varargs body env) :: IOThrowsError LispVal
makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc . Just . showVal
