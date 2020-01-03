{-# LANGUAGE ExistentialQuantification #-}
module LispTypes where

import           Control.Monad.Except
import           Text.ParserCombinators.Parsec
import           Data.Complex
import           Data.Array
import           Data.IORef
import           Text.Printf
import           System.IO

type IOThrowsError = ExceptT LispError IO

type Env = IORef [(String, IORef LispVal)]

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)


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
            | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
            | Func { params :: [String], vararg :: (Maybe String), body :: [LispVal], closure :: Env }
            | IOFunc ([LispVal] -> IOThrowsError LispVal)
            | Port Handle

instance Eq LispVal where
  (Atom x         ) == (Atom y         ) = x == y
  (List x         ) == (List y         ) = x == y
  (DottedList x x') == (DottedList y y') = x == y && x' == y'
  (Number    x    ) == (Number    y    ) = x == y
  (String    x    ) == (String    y    ) = x == y
  (Bool      x    ) == (Bool      y    ) = x == y
  (Character x    ) == (Character y    ) = x == y
  (Float     x    ) == (Float     y    ) = x == y
  (Ratio     x    ) == (Ratio     y    ) = x == y
  (Complex   x    ) == (Complex   y    ) = x == y
  (Vector    x    ) == (Vector    y    ) = x == y

data LispError = NumArgs Integer [LispVal]
                | TypeMismatch String LispVal
                | Parser ParseError
                | BadSpecialForm String LispVal
                | NotFunction String String
                | UnboundVar String String
                | Default String

unwordsVal :: [LispVal] -> String
unwordsVal = unwords . map showVal

showVal :: LispVal -> String
showVal (String contents     ) = "\"" ++ contents ++ "\""
showVal (Atom   name         ) = name
showVal (Number contents     ) = show contents
showVal (Bool   True         ) = "#t"
showVal (Bool   False        ) = "#f"
showVal (List   contents     ) = "(" ++ unwordsVal contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsVal head ++ showVal tail ++ ")"
showVal (PrimitiveFunc _     ) = "<primitive>"
showVal (Func { params = args, vararg = varargs, body = body, closure = env })
  = "(lambda ("
    ++ unwords (map show args)
    ++ (case varargs of
         Nothing  -> ""
         Just arg -> " . " ++ arg
       )
    ++ ") ...)"
showVal (Port   _) = "<IO Port>"
showVal (IOFunc _) = "<IO primitive>"

showError :: LispError -> String
showError (UnboundVar     message  varname) = message ++ ": " ++ varname
showError (BadSpecialForm message  form   ) = message ++ ": " ++ show form
showError (NotFunction    message  func   ) = message ++ ": " ++ show func
showError (NumArgs        expected found  ) = printf
  "Expected %i args; found %i values %s"
  expected
  (length found)
  (unwordsVal found)
showError (TypeMismatch expected found) =
  "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser  parseErr) = "Parse error at " ++ show parseErr
showError (Default s       ) = s


instance Show LispError where
  show = showError

instance Show LispVal where
  show = showVal

type ThrowsError = Either LispError
