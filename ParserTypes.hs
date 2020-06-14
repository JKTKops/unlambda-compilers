{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ParserTypes
    ( LispVal (..)
    , validFor90sc
    ) where

import Data.Maybe (fromMaybe)
import Data.Array

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Vector (Array Integer LispVal)
             | Number Integer
             | String String
             | Char Char
             | Bool Bool

instance Eq LispVal where (==) = eqVal
instance Show LispVal where show = showVal

validFor90sc :: LispVal -> Bool
validFor90sc v = case v of
    Atom _   -> True
    List vs  -> all validFor90sc vs
    DottedList vs u -> all validFor90sc vs && validFor90sc u
    Vector _ -> False
    Number _ -> True
    String _ -> False
    Char _   -> False
    Bool _   -> True

eqVal :: LispVal -> LispVal -> Bool
eqVal (Atom s) (Atom s') = s == s'
eqVal (Number n) (Number n') = n == n'
eqVal (String s) (String s') = s == s'
eqVal (Char c) (Char c') = c == c'
eqVal (Bool b) (Bool b') = b == b'
eqVal (List ls) (List ls') = ls == ls'
eqVal (DottedList ls l) (DottedList ls' l') = ls == ls' && l == l'
eqVal (Vector v) (Vector v') = v == v'
eqVal _ _ = False

showVal :: LispVal -> String
showVal (Atom s) = s
showVal (Number n) = show n
showVal (String s) = show s
showVal (Char c)   = "#\\" ++ case c of
    ' '  -> "space"
    '\t' -> "tab"
    '\n' -> "newline"
    '\r' -> "carriage-return"
    _    -> pure c
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List ls) = "(" ++ unwordsList ls ++ ")"
showVal (DottedList ls l) = "(" ++ unwordsList ls ++ " . " ++ show l ++ ")"
showVal (Vector v) = "#(" ++ unwordsList (elems v) ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show
