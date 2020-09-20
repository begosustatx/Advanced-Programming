-- Skeleton file for Boa Parser.

module BoaParser (ParseError, parseString) where
{- Grammer
Program ::= Stmts
Stmts ::= Stmt | Stmt `;" Stmts
Stmt ::= ident `=" Expr | Expr
Expr ::= numConst | stringConst | `None" | `True" | `False"
        | ident | Expr Oper Expr | `not" Expr | `(" Expr `)" 
        | ident `(" Exprz `)" | `[" Exprz `]" | `[" Expr ForClause Clausez `]"
Oper ::= `+" | `-" | `*" | `//" | `%" | `==" | `!=" 
        | `<" | `<=" | `>" | `>=" | `in" | `not" `in"
ForClause ::= `for" ident `in" Expr
IfClause ::= `if" Expr
Clausez ::= e | ForClause Clausez | IfClause Clausez
Exprz ::= e | Exprs
Exprs ::= Expr | Expr `," Exprs
ident ::= (see text)
numConst ::= (see text)
stringConst ::= (see text)
-}

import BoaAST
-- add any other other imports you need
import Data.Char (isDigit, isLetter)
import Control.Applicative ((<|>))
import Text.ParserCombinators.ReadP

type ParseError = String -- you may replace this

stmts = (do stmt; return ()) <|> 
        (do stmt; string ";"; stmts; return ())

stmt = (do ident; string "="; expr; return ();) <|> (do expr; return ())

expr = (do numConst; return ()) <|> (do stringConst; return()) <|> 
       (do string "None"; return ()) <|> (do string "True"; return()) <|> (do string "False"; return ()) <|> 
       (do stringConst; return()) <|> (do ident; return()) <|>  (do expr; oper; expr; return ()) <|> 
       (do string "not"; expr; return()) <|> (do string "("; expr; string ")"; return()) <|> 
       (do ident; string "("; exprz; string ")"; return()) <|> (do string "["; exprz; string "]"; return()) <|> 
       (do string "["; expr; forClause; clausez; string "]"; return())

oper = (do string "+"; return ()) <|> (do string "-"; return ()) <|> (do string "*"; return ())<|>
       (do string "//"; return ()) <|> (do string "%"; return ()) <|> (do string "=="; return ())<|>
       (do string "!="; return ()) <|> (do string "<"; return ()) <|> (do string "<="; return ())<|>
       (do string ">"; return ()) <|> (do string ">="; return ()) <|> (do string "in"; return ())<|>
       (do string "not"; string "in"; return ()) 

forClause = do string "for"
               ident
               string "in"
               expr
               return ()

ifClause = do string "if"
              expr
              return ()

clausez = return () <|> (do forClause
                            clausez 
                            return ())
                     <|>  (do ifClause
                              clausez
                              return ())


exprz = return () <|> (do exprs; return())

exprs = (do expr
            return ()) 
    <|> (do expr 
            string "," 
            exprs
            return ())

ident = undefined -- (see text)
numConst = undefined -- (see text)
stringConst = undefined -- (see text)

tokenize :: ReadP a -> ReadP a
tokenize p = skipSpaces >> p

parseString :: String -> Either ParseError () -- Program
parseString s = do
    case filter (null . snd ) (readP_to_S stmts s) of
        [] -> Left "No valid parse"
        [(exp, remainder)] ->
            if null remainder then Right exp
            else Left $ "Unparsed remainder:" ++remainder
        _ -> Left "Ambigius parse"