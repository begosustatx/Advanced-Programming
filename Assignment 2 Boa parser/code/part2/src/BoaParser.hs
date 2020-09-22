-- Skeleton file for Boa Parser.

module BoaParser (ParseError, parseString) where
{- Grammar
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
Digit   ::=  "0" | NonZero RestDigits
RestDigits ::= Digits RestDigits | e
NonZero ::= "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
Digits ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
stringConst ::=  " ' " Strings " ' "
Strings ::= Char Strings | e
Char ::= "A" | ... | "Z" | "a" | ... | "z" | "\\" | "\n" | "\’" | newline?
-}

import BoaAST
-- add any other other imports you need
import Data.Char (isDigit, isLetter, isAscii)
import Control.Applicative ((<|>))
import Text.ParserCombinators.ReadP

type ParseError = String -- you may replace this

program :: ReadP Program
program = tokenize stmts

stmts :: ReadP Program
stmts = do s<-stmt;endStmt s

stmt :: ReadP Stmt
-- tokenize $                     -set expr instead of finalExpr
stmt = (do v<-ident; string "="; e<-finalExpr; return $ SDef v e) <|> (do e<-finalExpr; return $ SExp e)

endStmt :: Stmt -> ReadP Program
endStmt s = tokenize $ (do string ";"; ss<-stmts; return (s:ss)) <|> (do return [s])

expr :: ReadP ()
expr = tokenize $ (do string "not"; expr; return()) <|> (do term; expr'; return ())

expr' :: ReadP ()
expr' = tokenize $ (do oper; term; expr'; return()) <|> (do return ())

term :: ReadP ()
term = tokenize $ (do string "("; expr; string ")"; return()) <|> (do ident; string "("; exprz; string ")"; return()) <|>
       (do string "["; exprz; string "]"; return()) <|> (do string "["; expr; fc<-forClause; clausez fc; string "]"; return())
       <|> (do finalExpr; return ())

finalExpr :: ReadP Exp
finalExpr = tokenize $ numConst <|> stringConst <|> (do string "None"; return $ Const NoneVal) <|> 
            (do string "True"; return $ Const TrueVal) <|> (do string "False"; return $ Const FalseVal) <|> 
            (do v<-ident;return $ Var v)

oper :: ReadP ()
oper = tokenize $ (do string "=="; return ()) <|> (do string "!="; return ()) <|> (do string "<"; return ()) <|> 
       (do string "<="; return ()) <|> (do string ">"; return ()) <|> (do string ">="; return ()) <|> 
       (do string "in"; return ()) <|> (do string "not"; string " in"; return ())  <|> (do arithOper; return ())

arithOper :: ReadP ()
arithOper = tokenize $ (do string "+"; return ()) <|> (do string "-"; return ()) <|> (do factOper; return ())

factOper :: ReadP ()
factOper = tokenize $ (do string "*"; return ()) <|> (do string "//"; return ()) <|> (do string "%"; return ())

forClause :: ReadP CClause
forClause = tokenize $ do string "for"
                          v<-ident
                          string "in"
                          e<-finalExpr --expr
                          return $ CCFor v e

ifClause :: ReadP CClause
ifClause = tokenize $  do string "if"
                          e<-finalExpr --expr
                          return $ CCIf e

clausez :: CClause -> ReadP [CClause]
clausez c = tokenize $ (do forc<-forClause; cs<-clausez forc;return (c:cs)) <|>
                       (do ifc<-ifClause; cs<-clausez ifc; return (c:cs))

exprz :: ReadP ()
exprz = tokenize $ (do exprs; return()) <|> return ()

exprs :: ReadP ()
exprs = tokenize $ (do expr; return ()) 
    <|> (do expr; string ","; exprs; return ())

newtype Keyword = Keyword String
                        deriving (Eq, Show, Read)
boaReservedWords :: [String]
boaReservedWords = ["None", "True", "False", "for", "if", "in", "not"]

varName :: ReadP String
varName = tokenize $ do
            fc <- firstChar
            rest <- many nonFirstChar
            return (fc:rest)
            where
               firstChar = satisfy (\a -> isLetter a || a == '_')
               nonFirstChar = satisfy (\a -> isDigit a || isLetter a || a == '_')

ident :: ReadP String -- (Either Keyword String) TODO see how to defined for keywords      
ident = do n <- varName 
           if n `elem` boaReservedWords then return "" -- $ Left (Keyword n)
           else return n -- $ Right (Var n)

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
stringConst = do str <- between (char' '\'') (char' '\'') (many stringChar)
                 return $ Const (StringVal str)
              where
                  stringChar = satisfy isAscii

tokenize :: ReadP a -> ReadP a
tokenize p = skipSpaces >> p
-- TODO add for # comments

char' :: Char -> ReadP Char 
char' = tokenize . char 

parseString :: String -> Either ParseError Program
parseString s = do
    case readP_to_S (do r<-program;eof >> return r) s of
        [] -> Left "No valid parse"
        [(exp, remainder)] ->
            if null remainder then Right exp
            else Left $ "Unparsed remainder:" ++remainder
        _ -> Left "Ambigius parse"