module WarmupReadP where

-- Original grammar (E is start symbol):
--   E ::= E "+" T | E "-" T | T | "-" T .
--   T ::= num | "(" E ")" .
-- Lexical specifications:
--   num is one or more decimal digits (0-9)
--   tokens may be separated by arbtrary whitespace (spaces, tabs, newlines).

-- Rewritten grammar, without left-recursion:
--   E  ::= T' E'
--   F  ::= "+" T F | "-" T F | ε .
--   T' ::= T | "-" T .
--   T  ::= num | "(" E ")" .

import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))
import Data.Char  (isDigit)
  -- may use instead of +++ for easier portability to Parsec

type Parser a = ReadP a   -- may use synomym for easier portability to Parsec

type ParseError = String  -- not particularly informative with ReadP

data Exp = Num Int | Negate Exp | Add Exp Exp
  deriving (Eq, Show)

tokenize :: Parser a -> Parser a
tokenize p = skipSpaces >> p

char' :: Char -> Parser Char 
char' = tokenize . char 

eP :: ReadP Exp
eP = do 
  e1 <- t'P
  e2 <- fP
  return $ e2 e1

fP :: ReadP (Exp->Exp)
fP = do  
    op <- satisfy (`elem` ['-','+'])
    e2 <- chainl1 tP plusP
    case op of
      '+' -> return $ \e1 -> Add e1 e2
      _ -> return $ \e1 -> Add e1 (Negate e2)
    where 
      plusP = tokenize $ do
                        op <- satisfy (`elem` ['-','+'])
                        case op of
                           '+' -> return $ \e1 e2 -> Add e1 e2
                           _ -> return $ \e1 e2 -> Add e1 (Negate e2)

t'P :: ReadP Exp
t'P = tP <|> do char' '-'
                n <- tP
                return (Negate n)

tP :: ReadP Exp 
tP = numberP <|> between (char' '(') (char' ')') eP

numberP :: ReadP Exp
numberP = tokenize $ do 
                   n <- many1 $ satisfy isDigit
                   return $ Num (read n)

parseString :: String -> Either ParseError Exp
parseString s = do
    case filter (null . snd ) (readP_to_S eP s) of
        [] -> Left "No valid parse"
        [(exp, remainder)] ->
            if null remainder then Right exp
            else Left $ "Unparsed remainder:" ++remainder
        _ -> Left "Ambigius parse"