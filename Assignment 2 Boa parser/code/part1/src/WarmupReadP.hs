module WarmupReadP where

-- Original grammar (E is start symbol):
--   E ::= E "+" T | E "-" T | T | "-" T .
--   T ::= num | "(" E ")" .
-- Lexical specifications:
--   num is one or more decimal digits (0-9)
--   tokens may be separated by arbitrary whitespace (spaces, tabs, newlines).

-- Rewritten grammar, without left-recursion:
--   E  ::= T' F
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

tokenize :: ReadP a -> ReadP a
tokenize p = skipSpaces >> p

char' :: Char -> ReadP Char 
char' = tokenize . char 

eP :: ReadP Exp
eP = tokenize $ do 
  e1 <- t'P
  r <- oprP e1 
  skipSpaces >> return r

oprP :: Exp -> ReadP Exp
oprP inval = (do _<-char' '-'; t <- tP; oprP (Add inval (Negate t))) <|> 
             (do _<-char' '+'; t <- tP; oprP (Add inval t)) <|> return inval

t'P :: ReadP Exp
t'P = tokenize (tP <|> (do _ <- char' '-'
                           n <- tP
                           return (Negate n)))

tP :: ReadP Exp 
tP = tokenize $ numberP <|> between (char' '(') (char' ')') eP

numberP :: ReadP Exp
numberP = tokenize $ do 
                   n <- many1 $ satisfy isDigit
                   return $ Num (read n)

parseString :: String -> Either ParseError Exp
parseString s = do
    case readP_to_S (do r<-eP;eof >> return r) s of
        [] -> Left "No valid parse"
        [(expr, remainder)] ->
            if null remainder then Right expr
            else Left $ "Unparsed remainder:" ++remainder
        _ -> Left "Ambigius parse"