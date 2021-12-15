{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Scheme
    ( LispVal (..), Scheme
    , validFor90sc
    ) where

import Data.Array

type Scheme = LispVal
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
showVal = ($ "") . showsVal

showsVal :: LispVal -> ShowS
showsVal (Atom s) = showString s
showsVal (Number n) = shows n
showsVal (String s) = showString $ show s
showsVal (Char c)   = showString $ "#\\" ++ case c of
    ' '  -> "space"
    '\t' -> "tab"
    '\n' -> "newline"
    '\r' -> "carriage-return"
    _    -> pure c
showsVal (Bool True) = showString "#t"
showsVal (Bool False) = showString "#f"
showsVal (List ls) = showChar '(' . unwordsList ls . showChar ')'
showsVal (DottedList ls l) = 
  showChar '(' . unwordsList ls
               . showString " . "
               . showsVal l
               . showChar ')'
showsVal (Vector v) = showString "#(" . unwordsList (elems v) . showChar ')'

unwordsList :: [LispVal] -> ShowS
unwordsList = unwords . map showsVal
  where unwords [] = id
        unwords ss = foldr1 (\s1 s2 -> s1 . showChar ' ' . s2) ss