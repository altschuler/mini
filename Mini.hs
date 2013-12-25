module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Control.Monad.Error
import System.Environment
import Numeric
import Utils

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

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

instance Show LispError where
  show (NumArgs expected found) = "Expected " ++ show expected
                                  ++ " args, found values " ++ show found
  show (UnboundVar message varname) = message ++ ": " ++ varname
  show (BadSpecialForm message form) = message ++ ": " ++ show form
  show (NotFunction message func) = message ++ ": " ++ show func
  show (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
  show (Parser parseErr) = "Parse error at " ++ show parseErr

instance Error LispError where
  noMsg = Default "An error occured :("
  strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

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
            <|> parseQuoted
            <|> do
              char '('
              x <- try parseList <|> parseDottedList
              char ')'
              return x

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                  ($ args)
                  (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [
  -- arithmetic ops
  ("+",         numericBinop (+)),
  ("-",         numericBinop (-)),
  ("*",         numericBinop (*)),
  ("/",         numericBinop div),
  ("mod",       numericBinop mod),
  ("quotient",  numericBinop quot),
  ("remainder", numericBinop rem),
  -- type checks
  ("string?",   isString),
  ("number?",   isNumber),
  ("symbol?",   isSymbol),
  -- bool ops
  ("=",         numBoolBinop (==)),
  ("<",         numBoolBinop (<)),
  (">",         numBoolBinop (>)),
  ("/=",        numBoolBinop (/=)),
  (">=",        numBoolBinop (>=)),
  ("<=",        numBoolBinop (<=)),
  ("&&",        boolBoolBinop (&&)),
  ("||",        boolBoolBinop (||)),
  ("string=?",  strBoolBinop (==)),
  ("string<?",  strBoolBinop (<)),
  ("string>?",  strBoolBinop (>)),
  ("string<=?", strBoolBinop (>)),
  ("string>=?", strBoolBinop (<=))]

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do
                               left  <- unpacker $ args !! 0
                               right <- unpacker $ args !! 1
                               return $ Bool $ left `op` right

numBoolBinop = boolBinop unpackNum                               
strBoolBinop = boolBinop unpackString
boolBoolBinop = boolBinop unpackBool

isString :: [LispVal] -> ThrowsError LispVal
isString [(String _)] = return $ Bool True
isString ((String _):xs) = isString xs >>= unpackBool >>= return . Bool
isString _ = return $ Bool False

isSymbol :: [LispVal] -> ThrowsError LispVal
isSymbol [(Atom _)] = return $ Bool True
isSymbol ((Atom _):xs) = isSymbol xs >>= unpackBool >>= return . Bool
isSymbol _ = return $ Bool False

isNumber :: [LispVal] -> ThrowsError LispVal
isNumber [(Number _)] = return $ Bool True
isNumber ((Number _):xs) = isNumber xs >>= unpackBool >>= return . Bool
isNumber _ = return $ Bool False

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool val) = return val
unpackBool notBool = throwError $ TypeMismatch "bool" notBool

unpackString :: LispVal -> ThrowsError String
unpackString (String val) = return val
unpackString notString = throwError $ TypeMismatch "string" notString

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number val) = return val
unpackNum notNum = throwError $ TypeMismatch "number" notNum

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

main :: IO ()
main = do
  args <- getArgs
  evaled <- return $ liftM show $ (readExpr (args !! 0) >>= eval)
  putStrLn $ extractValue $ trapError evaled
