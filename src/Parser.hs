module Parser (
  test
) where
import Eval
import Control.Monad
import Control.Monad.Combinators.Expr
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import qualified Data.Map.Strict as Map


-- import qualified Data.List as L
-- import qualified Data.Set as S
-- import qualified Data.Map as M
-- import qualified Data.Graph as G
import Data.Maybe (fromJust)

type Parser = Parsec Void String
sc :: Parser ()
sc = L.space (void spaceChar) (L.skipLineComment "#") (L.skipBlockComment "\"\"\"" "\"\"\"")

-- wrapper for my space consumr
lexeme = L.lexeme sc

symbol = L.symbol sc

underline = char '_'


lIdentifier :: Parser String

lIdentifier = try $ lexeme $ do
  smallcap <- lowerChar
  rest <- many (alphaNumChar <|> underline)
  let word = smallcap:rest
  if word `elem` rws then fail $ "you cannot use " ++ word ++" as a variable"
  else return word


uIdentifier :: Parser String

uIdentifier = try $ lexeme $ do
  bigcap <- upperChar
  rest <- many (alphaNumChar <|> underline)
  let word = bigcap:rest
  if word `elem` rws then fail $ "you cannot use " ++ word ++" as a type"
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
rws = ["if","then","else","True","False","&&","||", "\\", "->", "let", "in", "=", "+", "-", "*", "/", "<", ">", "==", "!=", "<=", ">=", "and"]


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
  <|> varExpr
  <|> lambdaExpr
  <|> letExpr
--  <|> dataOrTypeExpr
  <|> parens expr


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
  --var1 <- letDecl
  vars <- sepBy1 letDecl $ rword "and"
  rword "in"
  exp <- expr
  return $ ELet vars exp



oper :: Parser String
oper = do
   s <- lexeme $ many (oneOf "=-.:/\\~<>!@$%^&*+{}|?")
   return s

-- XD


wrapper :: Primitive -> Expr -> Expr -> Expr
wrapper p = \e1 e2 -> EApply (EApply (EData $ DPrimitive p 0 []) e1) e2
-- wrapper(__primitiveadd)




primitive_binary_iii :: (Int -> Int -> Int) -> Primitive
primitive_binary_iii fn = Primitive 2 (\[e1, e2] -> let
  e1v = case e1 of
    DInt x -> x
    _ -> undefined -- TODO: error
  e2v = case e2 of
    DInt x -> x
    _ -> undefined -- TODO: error
  in DInt $ fn e2v e1v
  )

__primitivemultiply = primitive_binary_iii (*)
__primitiveadd = primitive_binary_iii (+)
__primitivesubtract = primitive_binary_iii(-)

primitive_binary_bbb :: (Bool -> Bool -> Bool) -> Primitive
primitive_binary_bbb fn = Primitive 2 (\[e1, e2] -> let
  e1v = case e1 of
    DBool x -> x
    _ -> undefined -- TODO: error
  e2v = case e2 of
    DBool x -> x
    _ -> undefined -- TODO: error
  in DBool $ fn e1v e2v
  )

__primitiveor = primitive_binary_bbb (||)
__primitiveand = primitive_binary_bbb (&&)

primitive_binary_iib :: (Int -> Int -> Bool) -> Primitive
primitive_binary_iib fn = Primitive 2 (\[e1, e2] -> let
  e1v = case e1 of
    DInt x -> x
    _ -> undefined -- TODO: error
  e2v = case e2 of
    DInt x -> x
    _ -> undefined -- TODO: error
  in DBool $ fn e2v e1v
  )

__primitivele = primitive_binary_iib(<=)
__primitivelt = primitive_binary_iib(<)
__primitivegt = primitive_binary_iib(>)
__primitivege = primitive_binary_iib(>=)



primitive_binary_ii :: (Int -> Int) -> Primitive
primitive_binary_ii fn = Primitive 1 (\[e1, e2] -> let
  e1v = case e1 of
    DInt x -> x
    _ -> -1 -- TODO: error
  in DInt $ fn e1v
  )

__primitivenegate = primitive_binary_ii(0-)

--ręcznie

__primitivedivide = Primitive 2 $ \[e1, e2] -> let
  e1v = case e1 of
    DInt 0 -> undefined
    DInt x -> x
    _ -> undefined -- TODO: error
  e2v = case e2 of
   -- DInt 0 -> undefined -- TODO error
    DInt x -> x
    _ -> undefined -- TODO: error
  in DInt (div e2v  e1v)

__primitiveif = Primitive 3 $ \[e1, e2, e3]-> case e3 of
  DBool x -> if x then e2 else e1
  _ -> undefined -- TODO error

__primitiveeq = Primitive 2 $ \[e1, e2] -> case e1 of
  DInt x -> case e2 of
      DInt y -> DBool (x == y)
      _ -> undefined -- TODO error
  DBool x ->  case e2 of
      DBool y -> DBool (x == y)
      _ -> undefined -- TODO error

__primitiveneq = Primitive 2 $ \[e1, e2] -> case e1 of
  DInt x -> case e2 of
      DInt y -> DBool (x /= y)
      _ -> undefined -- TODO error
  DBool x ->  case e2 of
      DBool y -> DBool (x /= y)
      _ -> undefined -- TODO error


operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [
    {-[ prefix "-" (\e -> EApply (EData $ DPrimitive __primitivenegate 0 []) e)
    , prefix "+" id ],-}
   [ binaryL ""  EApply]
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


expr = makeExprParser expr' operatorTable


rec = "let factorial = \\n -> if n == 0 then 1  else n * factorial (n-1) in factorial 5"
ifs = "let if_t = \\b -> if b then 42 else 13 in if_t (30 < 50)"
bad1 = "let int_id :: Int -> Int = \\a -> a  in int_id True"
polymorph = "let id = \\x -> x in if (id True) then (id 1) else 0"
male = "30 < 50"
test =  case (parse expr "plik.txt" rec) of
  Right e -> print(eval  e Map.empty)
  Left _ -> putStrLn ("błąd XD")


--test = parseTest expr "let add_one = \\x -> x + 1 in add_one 12"

-- expr bez partial application i operatorów
-- top level: wielki expr
-- sprawdzanie błędów
-- złączenie wszystkiego