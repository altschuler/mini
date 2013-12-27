{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ExistentialQuantification #-}

module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import "mtl" Control.Monad.Error
import System.Environment
import System.IO
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

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

instance Show LispVal where
  show (Atom val) = show val
  show (Number val) = show val
  show (Float val) = show val
  show (String val) = "\"" ++ show val ++ "\""
  show (Character val) = "#\\" ++ show val
  show (Bool val) = if val then "#t" else "#f"
  show (List xs) = "(" ++ (unwords . map show) xs ++ ")"
  show (DottedList head tail) = "(" ++ show head ++ " . " ++ show tail ++ ")"

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

-- repl
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
evalString expr = return $ extractValue $ trapError (liftM show $ readExpr expr >>= eval)

evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "mini> ") evalAndPrint

-- compiler

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
eval (List (Atom "if" : xs)) = evalIf xs
eval (List (Atom "cond" : xs)) = evalCond xs
eval (List (Atom "case" : (key : xs))) = evalCase key xs
eval (List (Atom func : args)) = mapM eval args >>= apply func

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function" func)
                  ($ args)
                  (lookup func primitives)

evalCaseClause :: LispVal -> LispVal -> LispVal -> ThrowsError LispVal -> ThrowsError LispVal
evalCaseClause key val conseq alt = do
  result <- eqv [key, val]
  case result of
    Bool True  -> eval conseq
    Bool False -> alt
    otherwise  -> throwError $ TypeMismatch "boolean" result

evalCase :: LispVal -> [LispVal] -> ThrowsError LispVal
evalCase _ [List [Atom "else", conseq]] = eval conseq
evalCase key [List [val, conseq]] = evalCaseClause key val conseq (return $ List [])
evalCase key (List [val, conseq] : xs) = evalCaseClause key val conseq (evalCase key xs)
evalCase _ badArgList = throwError $ BadSpecialForm "invalid cond clause" (badArgList !! 0)

evalCondClause :: LispVal -> LispVal -> ThrowsError LispVal -> ThrowsError LispVal
evalCondClause pred conseq alt = do
  result <- eval pred
  case result of
    Bool True  -> eval conseq
    Bool False -> alt
    otherwise -> throwError $ TypeMismatch "boolean" result

evalCond :: [LispVal] -> ThrowsError LispVal
evalCond [List [Atom "else", conseq]] = eval conseq
evalCond [List [pred, conseq]] = evalCondClause pred conseq (return $ List [])
evalCond (List [pred, conseq] : xs) = evalCondClause pred conseq (evalCond xs)
evalCond badArgList = throwError $ BadSpecialForm "invalid cond clause" (badArgList !! 0)

evalIf :: [LispVal] -> ThrowsError LispVal
evalIf [pred, yes, no] = do
  result <- eval pred
  case result of
    Bool True  -> eval yes
    Bool False -> eval no
    otherwise  -> throwError $ TypeMismatch "boolean" result
evalIf badArgList = throwError $ NumArgs 3 badArgList

car :: [LispVal] -> ThrowsError LispVal
car [List (x:_)] = return x
car [DottedList (x:_) _] = return x
car [notList] = throwError $ TypeMismatch "list" notList
car notList = throwError $ NumArgs 1 notList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_:xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [notList] = throwError $ TypeMismatch "list" notList
cdr notList = throwError $ NumArgs 1 notList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []] = return $ List [x]
cons [x, List xs] = return $ List (x:xs)
cons [x, DottedList xs last] = return $ DottedList (x:xs) last
cons [x, y] = return $ DottedList [x] y
cons badArg = throwError $ NumArgs 2 badArg

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
  -- list handling
  ("car",       car),
  ("cdr",       cdr),
  ("cons",      cons),
  -- type checks
  ("string?",   isString),
  ("number?",   isNumber),
  ("symbol?",   isSymbol),
  ("eqv?",      eqv),
  ("equal?",    equal),
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

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1),       (Bool arg2)]       = return $ Bool $ arg1 == arg2
eqv [(Number arg1),     (Number arg2)]     = return $ Bool $ arg1 == arg2
eqv [(String arg1),     (String arg2)]     = return $ Bool $ arg1 == arg2
eqv [(Atom arg1),       (Atom arg2)]       = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List xs),         (List ys)]         = return $ Bool $ (length xs == length ys)
                                             && (all eqvPair (zip xs ys))
  where eqvPair (l, r) = case eqv [l, r] of
          Left err -> False
          Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

-- weakly typed equivalence
equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
  result <- liftM or $ mapM (unpackEquals arg1 arg2)
            [AnyUnpacker unpackNum, AnyUnpacker unpackString, AnyUnpacker unpackBool]
  eqvResult <- eqv [arg1, arg2]
  return $ Bool $ (result || let Bool x = eqvResult in x)
equal badArgList = throwError $ NumArgs 2 badArgList

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

checkType :: (LispVal -> Bool) -> [LispVal] -> ThrowsError LispVal
checkType f xs = return $ Bool $ all f xs

isString = checkType f
  where f (String _) = True
        f _ = False

isNumber = checkType f
  where f (Number _) = True
        f _ = False

isSymbol = checkType f
  where f (Atom _) = True
        f _ = False

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = do unpacked1 <- unpacker arg1
                                                   unpacked2 <- unpacker arg2
                                                   return $ unpacked1 == unpacked2
                                                `catchError` (const $ return False)

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

trapError action = catchError action (return . show)

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> runRepl
    1 -> evalAndPrint $ args !! 0
    otherwise -> putStrLn "Give me none or one args, please"
