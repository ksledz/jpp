module Parser (
  test,
  expr
) where
import Eval
import Control.Monad
import Control.Monad.Combinators.Expr
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import qualified Data.Map.Strict as Map



import Data.Maybe (fromJust)

type Parser = Parsec Void String
sc :: Parser ()
sc = L.space (void spaceChar) (L.skipLineComment "#") (L.skipBlockComment "\"\"\"" "\"\"\"")


lexeme = L.lexeme sc

symbol = L.symbol sc

underline = char '_'


lIdentifier :: Parser String

lIdentifier = try $ lexeme $ do
  smallcap <- lowerChar
  rest <- many (alphaNumChar <|> underline)
  let word = smallcap:rest
  if word `elem` rws then fail $ "special word " ++ word ++" cannot be used as a variable"
  else return word


uIdentifier :: Parser String

uIdentifier = try $ lexeme $ do
  bigcap <- upperChar
  rest <- many (alphaNumChar <|> underline)
  let word = bigcap:rest
  if word `elem` rws then fail $ "special word " ++ word ++" cannot be used as a type"
  else return $ bigcap:rest


parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integer :: Parser Int
integer = lexeme L.decimal

rword :: String -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy (alphaNumChar <|> underline))

rop :: String -> Parser ()
rop w = (lexeme . try) (string w *> notFollowedBy (oneOf "=-.:/\\~<>!@$%^&*+{}|?"))

rws :: [String] -- list of reserved words
rws = ["if","then","else","True","False", "undefined","&&","||", "\\", "->", "let", "in", "=", "+", "-", "*", "/", "<", ">", "==", "!=", "<=", ">=", "and"]


operDecl :: Parser (String, Expr)

operDecl = do
  op <- oper
  exp <- expr'
  return (op, exp)

expr :: Parser Expr

expr' :: Parser Expr
expr' = ifExpr
  <|> boolExpr
  <|> intExpr
  <|> undefinedExpr
  <|> varExpr
  <|> lambdaExpr
  <|> letExpr
  <|> parens expr


undefinedExpr :: Parser Expr
undefinedExpr = do
  rword "undefined"
  return $ EUndefined

boolExpr :: Parser Expr
boolExpr = (rword "True" *> pure (EData (DBool True))) <|> (rword "False" *> pure (EData (DBool False)))

intExpr :: Parser Expr
intExpr = do
  value <- integer
  return (EData (DInt value))

varExpr :: Parser Expr

varExpr = do
  ident <- lIdentifier
  return $ EVar ident

letDecl :: Parser (String, Expr)

letDecl = do
  name <- lIdentifier
  rword "="
  exp <- expr
  return (name, exp)


ifExpr :: Parser Expr
ifExpr = do
  rword "if"
  cond  <- expr
  rword "then"
  expr1 <- expr
  rword "else"
  expr2 <- expr
  return (EApply (EApply (EApply(EData (DPrimitive __primitiveif 0 [])) cond) expr1) expr2)

lambdaExpr :: Parser Expr
lambdaExpr = do
  rop "\\"
  names <- some lIdentifier
  rop "->"
  exp <- expr
  return $ foldr ELambda exp names

letExpr :: Parser Expr
letExpr = do
  rword "let"
  vars <- sepBy1 letDecl $ rword "and"
  rword "in"
  exp <- expr
  return $ ELet vars exp



oper :: Parser String
oper = do
   s <- lexeme $ many (oneOf "=-.:/\\~<>!@$%^&*+{}|?")
   return s



wrapper :: Primitive -> Expr -> Expr -> Expr
wrapper p = \e1 e2 -> EApply (EApply (EData $ DPrimitive p 0 []) e1) e2



primitive_binary_iii :: (Int -> Int -> Int) -> Primitive
primitive_binary_iii fn = Primitive 2 (\[e1, e2] -> do
  e1' <- e1
  e1v <- (case e1' of
    DInt x -> Right x
    _ -> Left "Invalid Type primitive binary iii"
    )
  e2' <- e2
  e2v <- (case e2' of
    DInt x -> Right x
    _ -> Left "Invalid Type primitive binary iii"
    )
  return $ DInt $ fn e2v e1v
  )

__primitivemultiply = primitive_binary_iii (*)
__primitiveadd = primitive_binary_iii (+)
__primitivesubtract = primitive_binary_iii(-)

primitive_binary_bbb :: (Bool -> Bool -> Bool) -> Primitive
primitive_binary_bbb fn = Primitive 2 (\[e1, e2] -> do
  e1' <- e1
  e1v <- case e1' of
    DBool x -> Right x
    _ -> Left "Invalid Type primitive binary bbb"
  e2' <- e2
  e2v <- case e2' of
    DBool x -> Right x
    _ -> Left "Invalid Type primitive binary bbb"
  return $ DBool $ fn e1v e2v
  )

__primitiveor = primitive_binary_bbb (||)
__primitiveand = primitive_binary_bbb (&&)

primitive_binary_iib :: (Int -> Int -> Bool) -> Primitive
primitive_binary_iib fn = Primitive 2 (\[e1, e2] -> do
  e1' <- e1
  e1v <- case e1' of
    DInt x -> Right x
    _ -> Left "Invalid Type primitive binary iib"
  e2' <- e2
  e2v <- case e2' of
    DInt x -> Right x
    _ -> Left "Invalid Type primitive binary iib"
  return $ DBool $ fn e2v e1v
  )

__primitivele = primitive_binary_iib(<=)
__primitivelt = primitive_binary_iib(<)
__primitivegt = primitive_binary_iib(>)
__primitivege = primitive_binary_iib(>=)




__primitivedivide = Primitive 2 $ \[e1, e2] -> do
  e1' <- e1
  e1v <- case e1' of
    DInt 0 -> Left "Divide by 0 :/"
    DInt x -> Right x
    _ -> Left "Invalid Type in division"
  e2' <- e2
  e2v <- case e2' of
    DInt x -> Right x
    _ -> Left "Invalid Type in division"
  return $ DInt (div e2v e1v)

__primitiveif = Primitive 3 $ \[e1, e2, e3]-> do
  e3' <- e3
  case e3' of
    DBool x -> if x then e2 else e1
    _ -> Left "Invalid Type in if"


__primitiveeq = Primitive 2 $ \[e1, e2] -> do
  e1' <- e1
  e2' <- e2

  case e1' of
    DInt x -> case e2' of
      DInt y -> Right (DBool (x == y))
      _ -> Left "Invalid Type in =="
    DBool x ->  case e2' of
      DBool y -> Right (DBool (x == y))
      _ -> Left "Invalid Type in =="

__primitiveneq = Primitive 2 $ \[e1, e2] -> do

  e1' <- e1
  e2' <- e2

  case e1' of
    DInt x -> case e2' of
      DInt y -> Right (DBool (x /= y))
      _ -> Left "Invalid Type in !="
    DBool x ->  case e2' of
      DBool y -> Right (DBool (x /= y))
      _ -> Left "Invalid Type in !="

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [
   [ binaryL "" EApply]
  , [ binaryL "*" (wrapper __primitivemultiply)
    , binaryL "/" (wrapper __primitivedivide) ]
  , [ binaryL "+" (wrapper __primitiveadd)
    , binaryL "-" (wrapper __primitivesubtract) ]
  , [ binaryL "<" (wrapper __primitivelt)
    , binaryL ">" (wrapper __primitivegt)
    , binaryL ">=" (wrapper __primitivege)
    , binaryL "<=" (wrapper __primitivele)
    , binaryL "!=" (wrapper __primitiveneq)
    , binaryL "==" (wrapper __primitiveeq) ]
  , [ binaryR "||" (wrapper __primitiveor)
    , binaryR "&&" (wrapper __primitiveand)  ]
  ]

binaryL :: String -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binaryL  name f = InfixL  (f <$ rop name)

binaryR :: String -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binaryR  name f = InfixR  (f <$ rop name)

prefix, postfix :: String -> (Expr -> Expr) -> Operator Parser Expr
prefix  name f = Prefix  (f <$ rop name)
postfix name f = Postfix (f <$ rop name)


expr = makeExprParser (sc >> expr') operatorTable


rec = "let factorial = \\n -> if n == 0 then 1  else n * factorial (n-1) in factorial 5"
bad2 = "let else = 7 in else"
ifs = "let if_t = \\b -> if b then 42 else 13 in if_t (30 < 50)"
bad1 = "let int_id :: Int -> Int = \\a -> a  in int_id True"
polymorph = "let id = \\x -> x in if (id True) then (id 1) else 0"
male = "30 < 50"
test =   print(parse expr "plik.txt" male)
