-- Skeleton file for Boa Parser.

module BoaParser (ParseError, parseString) where
{- Grammar
Program ::= Stmts
Stmts ::= Stmt endStmt
endStmt ::= ";" Stmts | e
Stmt ::= ident "=" Expr | Expr
Expr ::= "not" Expr | Expr'
Expr' ::= Term Oper Expr' | Term
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
-- tokenize $                     
stmt = (do v<-ident; char' '='; e<-expr; return $ SDef v e) <|> (do e<-expr; return $ SExp e)

endStmt :: Stmt -> ReadP Program
endStmt s = tokenize $ (do char' ';'; ss<-stmts; return (s:ss)) <|> (do return [s])

expr :: ReadP Exp -- without space solution for Expression misc
expr = tokenize $ (do string "not "; e<-expr; return $ Not e) <|> expr'

expr' :: ReadP Exp
expr' = tokenize $ do t<-term; oper t

-- SKIPPED
term :: ReadP Exp
term = tokenize $ (do between (char' '(') (char' ')') expr) <|> termCall <|> termList <|>
      (do char' '['; e<-expr; fc<-forClause; c<-clausez fc; char' ']'; return $ Compr e c) <|> finalExpr

termCall :: ReadP Exp
termCall = do v<-ident
              char' '('
              e<-exprz
              char' ')'
              return $ Call v e

termList :: ReadP Exp
termList = do char' '['
              e<-exprz
              char' ']'
              return $ List e

finalExpr :: ReadP Exp
finalExpr = tokenize $ numConst <|> stringConst <|> (do string "None"; return $ Const NoneVal) <|> 
            (do string "True"; return $ Const TrueVal) <|> (do string "False"; return $ Const FalseVal) <|> 
            (do v<-ident;return $ Var v)

-- SKIPPED
oper :: Exp -> ReadP Exp
oper inval = tokenize $ (do string "=="; e<-expr'; oper (Oper Eq inval e)) <|> (do string "!="; e<-expr'; e1<-oper (Oper Eq inval e);return $ Not e1)
                <|> (do string "<"; e<-expr'; oper (Oper Less inval e)) <|> (do string ">";  e<-expr'; oper (Oper Greater inval e))
                <|> (do string ">="; e<-expr'; e1<-oper (Oper Less inval e);return $ Not e1) <|> (do string "<="; e<-expr'; e1<-oper (Oper Greater inval e);return $ Not e1)
                <|> (do string "in "; e<-expr'; oper (Oper In inval e)) <|> (do string "not"; string " in"; e<-expr'; e1<-oper (Oper In inval e); return $ Not e1)
                <|> arithOper inval

arithOper :: Exp -> ReadP Exp
arithOper inval = tokenize $ (do string "+"; e<-expr'; arithOper (Oper Plus inval e)) <++ 
                             (do string "-"; e<-expr'; arithOper (Oper Minus inval e)) <++ factOper inval

factOper :: Exp -> ReadP Exp
factOper inval = tokenize $ (do string "*"; e<-expr'; factOper (Oper Times inval e)) <++ 
                      (do string "//"; e<-expr'; factOper (Oper Div inval e)) <++ 
                      (do string "%"; e<-expr'; factOper (Oper Mod inval e)) <++ return inval

forClause :: ReadP CClause
forClause = tokenize $ do string "for "
                          v<-ident
                          skipSpaces >> string "in "
                          e<-expr
                          return $ CCFor v e

ifClause :: ReadP CClause
ifClause = tokenize $  do string "if "
                          e<-expr
                          return $ CCIf e

clausez :: CClause -> ReadP [CClause]
clausez c = tokenize $ (do forc<-forClause; cs<-clausez forc;return (c:cs)) <|>
                       (do ifc<-ifClause; cs<-clausez ifc; return (c:cs)) <|> return [c]

exprz :: ReadP [Exp]
exprz = tokenize $ (do e<-exprs;return $ e) <|> return []

exprs :: ReadP [Exp]
exprs = tokenize $ (do e<-expr;return [e]) <|> (do e<-expr; string ","; es<-exprs; return $ e:es)

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

ident :: ReadP String 
ident = do n <- varName 
           if n `elem` boaReservedWords then pfail 
           else return n

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