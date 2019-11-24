module Main where

import           Numeric
import           Lib
import           System.Environment
import           Data.Char
import           Data.Ratio
import           Data.Complex
import           Data.Array
import           Text.ParserCombinators.Parsec
                                         hiding ( spaces )

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             | Float Double
             | Ratio Rational
             | Complex (Complex Double)
             | Vector (Array Int LispVal)
             deriving (Show)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (escapedChars <|> noneOf "\"")
  char '"'
  return $ String x

parseBool :: Parser LispVal
parseBool = do
  char '#'
  x <- oneOf "tf"
  return $ case x of
    't' -> Bool True
    'f' -> Bool False

escapedChars :: Parser Char
escapedChars = do
  char '\\'
  x <- oneOf "\\\"nrt"
  return $ case x of
    '\\' -> x
    '\"' -> '"'
    'n'  -> '\n'
    'r'  -> '\r'
    't'  -> '\t'

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest  <- many (letter <|> symbol <|> digit)
  return $ Atom (first : rest)

parseChar :: Parser LispVal
parseChar = do
  string "#\\"
  value <- many1 alphaNum
  if length value == 1
    then return $ Character (head value)
    else case (map toLower value) of
      "space"   -> return $ Character ' '
      "newline" -> return $ Character '\n'
      _         -> fail "Invalid character literal"


parseDecimal1 :: Parser LispVal
parseDecimal1 = many1 digit >>= return . Number . read

parseDecimal2 :: Parser LispVal
parseDecimal2 = do
  try $ string "#d"
  parseDecimal1

parseOct :: Parser LispVal
parseOct = do
  try $ string "#o"
  x <- many1 octDigit
  return $ Number (oct2dig x)
  where oct2dig y = fst $ readOct y !! 0

parseHex :: Parser LispVal
parseHex = do
  try $ string "#h"
  x <- many1 hexDigit
  return $ Number (hex2dig x)
  where hex2dig y = fst $ readHex y !! 0

parseBin :: Parser LispVal
parseBin = do
  try $ string "#b"
  x <- many1 digit
  return $ Number (bin2dig x)
 where
  bin2dig = bin2dig' 0
  bin2dig' n "" = n
  bin2dig' n (x : xs) =
    let current = (n * 2) + (if x == '1' then 1 else 0) in bin2dig' current xs

parseInteger :: Parser LispVal
parseInteger =
  parseDecimal1 <|> parseDecimal2 <|> parseOct <|> parseHex <|> parseBin

parseFloat :: Parser LispVal
parseFloat = do
  ds <- many1 digit
  char '.'
  decs <- many1 digit
  return $ Float $ toFloat (ds ++ "." ++ decs)
  where toFloat xs = fst $ readFloat xs !! 0

parseRatio :: Parser LispVal
parseRatio = do
  numerator <- many1 digit
  char '/'
  denominator <- many1 digit
  return $ Ratio (read numerator % read denominator)

toDouble :: LispVal -> Double
toDouble (Float  x) = x
toDouble (Number x) = fromIntegral x

parseComplex :: Parser LispVal
parseComplex = do
  r <- try parseFloat <|> parseInteger
  char '+'
  i <- try parseFloat <|> parseInteger
  char 'i'
  return $ Complex (toDouble r :+ toDouble i)

parseNumber :: Parser LispVal
parseNumber =
  try parseComplex <|> try parseFloat <|> try parseRatio <|> parseInteger

parseList :: Parser LispVal
parseList = do
  values <- sepBy parseExpr spaces
  return $ List values

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseLists :: Parser LispVal
parseLists = do
  char '('
  value <- try parseList <|> parseDottedList
  char ')'
  return value

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  exp <- parseExpr
  return $ List [Atom "quote", exp]

parseQuasiquote :: Parser LispVal
parseQuasiquote = do
  char '`'
  exp <- parseExpr
  return $ List [Atom "quasiquote", exp]

parseUnquote :: Parser LispVal
parseUnquote = do
  char ','
  exp <- parseExpr
  return $ List [Atom "unquote", exp]

parseVector :: Parser LispVal
parseVector = do
  string "#("
  values <- sepBy parseExpr spaces
  char ')'
  return $ Vector (listArray (0, (length values) - 1) values)

parseQuotes :: Parser LispVal
parseQuotes = parseQuoted <|> parseQuasiquote <|> parseUnquote

parseExpr :: Parser LispVal
parseExpr =
  parseAtom
    <|> parseString
    <|> try parseBool
    <|> try parseVector
    <|> parseNumber
    <|> parseQuotes
    <|> parseLists

readExpr :: String -> String
readExpr input = case parse parseExpr "Lisp" input of
  Left  err -> "No match: " ++ show err
  Right val -> "Found value: " ++ show val

parseIt :: String -> Maybe LispVal
parseIt s = case (parse parseExpr "lisp" s) of
  Left  _ -> Nothing
  Right s -> Just s


main :: IO ()
main = do
  (arg : _) <- getArgs
  putStrLn $ readExpr arg
