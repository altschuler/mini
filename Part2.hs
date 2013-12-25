module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import System.Environment
import Numeric
import Utils

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Float
             | String String
             | Character Char
             | Bool Bool

instance Show LispVal where
  show (Atom val) = "[atom:" ++ show val ++ "]"
  show (Number val) = "[number:" ++ show val ++ "]"
  show (Float val) = "[float:" ++ show val ++ "]"
  show (String val) = "[string:" ++ show val ++ "]"
  show (Character val) = "[char:" ++ show val ++ "]"
  show (Bool val) = "[bool:" ++ show val ++ "]"
  show (List (x:xs)) = "[list:(" ++ show x ++ " " ++ show xs ++ ")]"
  show (DottedList head tail) = "[dotlist: head(" ++ show head ++ "), tail(" ++ show tail ++ ")]"

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

escaped :: Parser String
escaped = do
  char '\\'
  x <- oneOf "\\\"nrt"
  case x of
    '\\' -> do return [x]
    '"'  -> do return [x]
    'n'  -> do return "\n"
    'r'  -> do return "\r"
    't'  -> do return "\t"

parseBool :: Parser LispVal
parseBool = do
  try $ string "#"
  x <- oneOf "tf"
  return $ case x of
    't' -> Bool True
    'f' -> Bool False

parseString :: Parser LispVal
parseString = do
  try $ char '"'
  x <- many $ many1 (noneOf "\"\\") <|> escaped
  char '"'
  return $ String (concat x)

parseCharacter :: Parser LispVal
parseCharacter = do
  try $ string "#\\"
  x <- anyChar
  return $ Character x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

parseSimpleNumber :: Parser LispVal
parseSimpleNumber = many1 digit >>= return . Number . read

parseDec :: Parser LispVal
parseDec = try $ string "#d" >> parseSimpleNumber

parseHex :: Parser LispVal
parseHex = do
  try $ string "#x"
  x <- many1 hexDigit
  return $ Number (fst $ readHex x !! 0)

parseOct :: Parser LispVal
parseOct = do
  try $ string "#o"
  x <- many1 octDigit
  return $ Number (fst $ readOct x !! 0)

parseBin :: Parser LispVal
parseBin = do
  try $ string "#b"
  x <- many1 (oneOf "01")
  return $ Number (bin2dec x)

parseNumber :: Parser LispVal
parseNumber = parseSimpleNumber
              <|> parseDec
              <|> parseHex
              <|> parseOct
              <|> parseBin

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]



parseExpr :: Parser LispVal
parseExpr = parseAtom
            <|> parseString
            <|> parseCharacter
            <|> parseNumber
            <|> parseBool
            <|> do
              char '('
              x <- try parseList <|> parseDottedList
              char ')'
              return x

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found: " ++ show val

main :: IO ()
main = do
  args <- getArgs
  putStrLn (readExpr $ args !! 0)
