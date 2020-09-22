-- Skeleton file for Boa Parser.

module BoaParser (ParseError, parseString) where
{- Grammer
Program ::= Stmts
Stmts ::= Stmt endStmt
endStmt ::= ";" Stmts | e
Stmt ::= ident "=" Expr | Expr
Expr ::= "not" Expr | Term Expr'
Expr'  ::= Oper Term Expr' | e
Term :: = "(" Expr ")" | ident "(" Exprz ")" | "[" Exprz "]" | "[" Expr ForClause Clausez "]" | FinalExpr
FinalExpr :: = numConst | stringConst | "None" | "True" | "False" | ident
Oper ::= "==" | "!=" | "<" | "<=" | ">" | ">=" | "in" | "not" "in" | ArithOper
ArithOper  ::= "+" | "-" | FactOper
FactOper ::= "*" | "//" | "%"  
ForClause ::= "for" ident "in" Expr
IfClause ::= "if" Expr
Clausez ::= e | ForClause Clausez | IfClause Clausez
Exprz ::= e | Exprs
Exprs ::= Expr | Expr "," Exprs
ident ::= Letter Letdigs
Letdigs ::= Letter Letdigs | Digits Letdigs | e
Letter ::= "_" | "A" | ... | "Z" | "a" | ... | "z"
numConst ::= "-" Digit | Digit
Digit   ::=  "0" | NonZero Digits 
NonZero ::= "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
Digits ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
stringConst ::= "'" Letters "'" -- and other
Letters ::= Letter Letters
Letter ::= "a" | ... | "Z" | e
-}

import BoaAST
-- add any other other imports you need
import Data.Char (isDigit, isLetter)
import Control.Applicative ((<|>))
import Text.ParserCombinators.ReadP

type ParseError = String -- you may replace this

program = tokenize $ (do stmts; return ()) 

stmts = tokenize $ (do stmt; endStmt; return ())

stmt = tokenize $ (do ident; string "="; expr; return ()) <|> (do expr; return ())

endStmt = tokenize $ (do string ";"; stmts; return ()) <|> (do return ())

expr = tokenize $ (do string "not"; expr; return()) <|> (do term; expr'; return ())

expr' = tokenize $ (do oper; term; expr'; return()) <|> (do return ())

term = tokenize $ (do string "("; expr; string ")"; return()) <|> (do ident; string "("; exprz; string ")"; return()) <|>
       (do string "["; exprz; string "]"; return()) <|> (do string "["; expr; forClause; clausez; string "]"; return())
       <|> (do finalExpr; return ())

finalExpr = tokenize $ (do numConst; return ()) <|> (do stringConst; return()) <|> (do string "None"; return ()) <|>
            (do string "True"; return()) <|> (do string "False"; return ()) <|> (do ident; return()) 

oper = tokenize $ (do string "=="; return ()) <|> (do string "!="; return ()) <|> (do string "<"; return ()) <|> 
       (do string "<="; return ()) <|> (do string ">"; return ()) <|> (do string ">="; return ()) <|> 
       (do string "in"; return ()) <|> (do string "not"; string "in"; return ())  <|> (do arithOper; return ())

arithOper = tokenize $ (do string "+"; return ()) <|> (do string "-"; return ()) <|> (do factOper; return ())

factOper = tokenize $ (do string "*"; return ()) <|> (do string "//"; return ()) <|> (do string "%"; return ())

forClause = tokenize $ do string "for"
                          ident
                          string "in"
                          expr
                          return ()

ifClause = tokenize $  do string "if"
                          expr
                          return ()

clausez = tokenize $ (do forClause; clausez; return ()) <|>
                (do ifClause; clausez; return ()) <|> return ()

exprz = tokenize $ (do exprs; return()) <|> return ()

exprs = tokenize $ (do expr; return ()) 
    <|> (do expr; string ","; exprs; return ())

data Keyword = Keyword String
                      deriving (Eq, Show, Read)
boaReservedWords :: [String]
boaReservedWords = ["None", "True", "False", "for", "if", "in", "not"]

varName :: ReadP String
varName = do
            fc <- firstChar
            rest <- many1 nonFirstChar
            return (fc:rest)
            where
               firstChar = satisfy (\a -> isLetter a || a == '_')
               nonFirstChar = satisfy (\a -> isDigit a || isLetter a || a == '_')

ident :: ReadP (Either Keyword Exp)        
ident = do n <- varName 
           if n `elem` boaReservedWords then return $ Left (Keyword n)
           else return $ Right (Var n)

-- TODO represent negatives
numConst :: ReadP Exp
numConst = tokenize $ do
                     first <- (do minus;digit) <|> digit
                     case first of
                             '0' -> return $ Const (IntVal (read [first]))
                             _ -> do rest <- restDigits
                                     return $ Const (IntVal (read (first:rest)))
                     where
                          minus = char '-'
                          nonZero n = n `elem` ['1'..'9']
                          digit = satisfy (\n-> nonZero n || n=='0')
                          restDigits = many $ satisfy isDigit
-- TODO newline? e?
stringConst :: ReadP Exp
stringConst = do str <- between (char' '\'') (char' '\'') (many $ stringChar)
                 return $ Const (StringVal str)
              where
                  stringChar = satisfy (\a -> isLetter a || a == '\\' || a=='\n' || a=='\'' || a==' ')

tokenize :: ReadP a -> ReadP a
tokenize p = skipSpaces >> p
-- TODO add for # comments

char' :: Char -> ReadP Char 
char' = tokenize . char 

parseString :: String -> Either ParseError () -- Program
parseString s = do
    case (readP_to_S (do r<-program;eof >> return r) s) of
        [] -> Left "No valid parse"
        [(exp, remainder)] ->
            if null remainder then Right exp
            else Left $ "Unparsed remainder:" ++remainder
        _ -> Left "Ambigius parse"