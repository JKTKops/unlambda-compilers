module Language.SchemeParser where

import Text.Parsec hiding (Error, try, spaces, parse)
import qualified Text.Parsec as Parsec (parse)
import Text.Parsec.Char (octDigit, hexDigit)
import Text.Parsec.Language (emptyDef)
import Text.ParserCombinators.Parsec hiding (spaces, parse)
import qualified Text.Parsec.Token as Tok

import Data.Char (digitToInt)
import Data.Array (listArray)

import Language.Scheme

parse :: String -> String -> Either ParseError LispVal
parse fn inp = List . (Atom "begin" :) <$> labeledReadExprList fn inp

parse' :: String -> String -> LispVal
parse' origin src = case parse origin src of
    Right v  -> v
    Left err -> error $ show err

quoteScheme :: String -> [LispVal]
quoteScheme s = case parse' "<compiler source>" s of
    List (_:v) -> v -- throw away the "begin"

labeledReadExprList :: String -> String -> Either ParseError [LispVal]
labeledReadExprList label = labeledReadOrThrow label $ do
    whiteSpace
    exprs <- many parseExpr
    eof
    return exprs

labeledReadOrThrow :: String -> Parser a -> String -> Either ParseError a
labeledReadOrThrow label parser input = Parsec.parse parser label input

readOrThrow :: Parser a -> String -> Either ParseError a
readOrThrow = labeledReadOrThrow "Thcheme"

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where style = emptyDef { Tok.commentStart = "#|"
                         , Tok.commentEnd   = "|#"
                         , Tok.commentLine  = ";"
                         , Tok.identStart   = letter <|> symbol
                         , Tok.identLetter  = letter <|> digit <|> symbol
                         }

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer

identifier :: Parser String
identifier = Tok.identifier lexer

stringLiteral :: Parser String
stringLiteral = Tok.stringLiteral lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

anyBraces :: Parser a -> Parser a
anyBraces p = parens p
          <|> Tok.braces lexer p
          <|> Tok.brackets lexer p

decimal, hexadecimal, octal :: Parser Integer
decimal     = Tok.decimal lexer
hexadecimal = Tok.hexadecimal lexer
octal       = Tok.octal lexer

float :: Parser Double
float = Tok.float lexer

lexeme :: Parser a -> Parser a
lexeme = Tok.lexeme lexer

-- the absurd number of parens is because someone thought <?> should be infix 0
parseExpr :: Parser LispVal
parseExpr = lexeme $
             (try parseNumber <?> "number")
             -- atom names can start with #\ too so we need an @try@
         <|> (try parseChar <?> "character literal")
         <|> (try parseVector <?> "vector")
         <|> (parseAtom <?> "symbol")
         <|> (parseString <?> "string")
         <|> (parseQuoted <?> "quote form")
         <|> (parseListLike <?> "list")

symbol :: Parser Char
symbol = oneOf "!@#$%^&*-_=+|:\\/?<>~"

spaces :: Parser ()
spaces = skipMany1 space

delim :: Parser ()
delim = notFollowedBy $ alphaNum <|> symbol

parseString :: Parser LispVal
parseString = String <$> stringLiteral

parseAtom :: Parser LispVal
parseAtom = do
    atom <- identifier
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = lexeme $ do
    sign <- optionMaybe $ char '-' <|> char '+'
    prefix <- parseRadixPrefix <|> return "#d"
    num <- Number <$> case prefix of
        "#b" -> number 2 (char '0' <|> char '1')
        "#o" -> number 8 octDigit
        "#d" -> number 10 digit -- float case goes here :)
        "#h" -> number 16 hexDigit
    delim
    case sign of
        Nothing  -> return num
        Just '+' -> return num
        Just '-' -> let Number n = num in return . Number $ negate n

  where parseRadixPrefix :: Parser String
        parseRadixPrefix = do
            hash <- char '#'
            prefix <- oneOf "bodh"
            return [hash, prefix]

        number base baseDigit = do
            digits <- many1 baseDigit
            let n = foldl (\x d -> base * x + toInteger (digitToInt d)) 0 digits
            seq n (return n)

parseChar :: Parser LispVal
parseChar = lexeme $ do
    prefix <- string "#\\"
    char   <- do try $ string "space"
                 return ' '
              <|> (do try $ string "newline"
                      return '\n')
              <|> (do try $ string "carriage-return"
                      return '\r')
              <|> (do try $ string "tab"
                      return '\t')
              <|> try (do c <- anyChar
                          delim
                          return c)
              <?> "char literal"
    return $ Char char

parseVector :: Parser LispVal
parseVector = lexeme $ do
    char '#'
    exprs <- parens $ many parseExpr
    return . Vector . listArray (0, fromIntegral $ length exprs - 1) $ exprs

--parseList :: Parser LispVal
--parseList = List <$> many parseExpr -- sepBy parseExpr (skipMany space)

parseListLike :: Parser LispVal
parseListLike = parens $ do
  init <- many parseExpr
  last <- optionMaybe $ lexeme (char '.') >> parseExpr
  case (init, last) of
    (xs, Nothing) -> return $ List xs
    ([], Just v)  -> return v
    (init, Just (List ls)) -> return $ List $ init ++ ls
    (init, Just val)     -> return $ DottedList init val

--parseDottedList :: Parser LispVal
--parseDottedList = do
--    init <- many parseExpr
--    last <- lexeme (char '.') >> parseExpr
--    case (init, last) of
--        ([], _)         -> return last
--        (init, List ls) -> return . List $ init ++ ls
--        (init, val)     -> return $ DottedList init val

parseQuoted :: Parser LispVal
parseQuoted = lexeme $ foldr1 (<|>) parsers
  where parsers = map (\sym -> do
            try $ string sym
            let macro = case sym of
                    "'"  -> "quote"
                    "`"  -> "quasiquote"
                    ","  -> "unquote"
                    ",@" -> "unquote-splicing"
            x <- parseExpr
            return $ List [Atom macro, x]) ["'", "`", ",@", ","]
