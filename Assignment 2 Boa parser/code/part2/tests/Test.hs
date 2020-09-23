-- Rudimentary test suite. Feel free to replace anything.

import BoaAST
import BoaParser

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests :: TestTree
tests = testGroup "Tests" [stubbyTests, unitTests]

stubbyTests = testGroup "Minimal tests" [
  testCase "simple success" $
    parseString "2 + two" @?=
      Right [SExp (Oper Plus (Const (IntVal 2)) (Var "two"))],
  testCase "simple failure" $
    -- avoid "expecting" very specific parse-error messages
    case parseString "wow!" of
      Left e -> return ()  -- any message is OK
      Right p -> assertFailure $ "Unexpected parse: " ++ show p]

unitTests = testGroup "Unit Tests" [identifiersTests, stringTest, numberTests,
                                    relOperTests, arithOperTests, factorOperTests, mixOperTests,
                                    finalExprTests]--, clauseTests, callTests, listTests,
                                    --stmtTests, stmtsTests] 

identifiersTests = testGroup "identifiers Tests"
  [testCase "ident x - Var x" $
     parseString "x" @?= Right [SExp (Var "x")],
   testCase "ident _ - Var _" $
     parseString "_" @?= Right [SExp (Var "_")],
   testCase "ident x1 - Var x1" $
     parseString "x1" @?= Right [SExp (Var "x1")],
   testCase "ident x_1 - Var x_1" $
     parseString "x_1" @?= Right [SExp (Var "x_1")],
   testCase "ident x11 - Var x11" $
     parseString "x11" @?= Right [SExp (Var "x11")],
   testCase "ident _1 - Var _1" $
     parseString "_1" @?= Right [SExp (Var "_1")],
   testCase "ident 1x - error Not Var" $
     parseString "1x" @?= Left "No valid parse",
   testCase "ident x - Var x" $
     parseString "" @?= Left "No valid parse",
   testCase "ident for - error in parse keyword" $
     parseString "for" @?= Left "No valid parse",
   testCase "ident if - error in parse keyword" $
     parseString "if" @?= Left "No valid parse",
   testCase "ident in - error in parse keyword" $
     parseString "in" @?= Left "No valid parse",
   testCase "ident not - error in parse keyword" $
     parseString "not" @?= Left "No valid parse"]

stringTest = testGroup "string Tests"
  [testCase "string '' - StringVal ''" $
     parseString "''" @?= Right [SExp (Const (StringVal ""))],
   testCase "string 'text' - StringVal 'text'" $
     parseString "'text'" @?= Right [SExp (Const (StringVal "text"))],
   testCase "string '\'text@' - No valid parse" $
     parseString "'text@" @?= Left "No valid parse",
   testCase "string '' - StringVal 'text@'" $
     parseString "'text@'" @?= Right [SExp (Const (StringVal "text@"))],
   testCase "string '' - StringVal ''" $
     parseString "''"  @?= Right [SExp (Const (StringVal ""))],
   testCase "string 'text   ' - StringVal 'text   '" $
     parseString "'text   '" @?= Right [SExp (Const (StringVal "text   "))],
   testCase "string 'asd\'sa' - StringVal 'asd\'sa'" $
     parseString "'asd\'sa'" @?= Right [SExp (Const (StringVal "asd\'sa"))],
   testCase "string 'text\nt' - StringVal 'text\nt'" $
     parseString "'text\nt'" @?= Right [SExp (Const (StringVal "text\nt"))],
   testCase "string 'asd\\sa' - StringVal 'asd\\sa'" $
     parseString "'asd\\sa'" @?= Right [SExp (Const (StringVal "asd\\sa"))],
   testCase "string 'asd//sa' - StringVal 'asd//sa'" $
     parseString "'asd//sa'" @?= Right [SExp (Const (StringVal "asd//sa"))],
   testCase "string '' - StringVal ''" $
     parseString "'\"#~ '" @?= Right [SExp (Const (StringVal "\"#~ "))]]


numberTests = testGroup "number Tests"
  [testCase "number 0 - IntVal 0" $
     parseString "0" @?= Right [SExp (Const (IntVal 0))],
   testCase "number -0 - IntVal 0" $
     parseString "-0" @?= Right [SExp (Const (IntVal 0))],
   testCase "number 01 - Error" $
     parseString "01" @?= Left "No valid parse",
   testCase "number 00 - Error" $
     parseString "00" @?= Left "No valid parse",
   testCase "number 001 - Error" $
     parseString "001"  @?= Left "No valid parse",
   testCase "number 1 - IntVal 1" $
     parseString "1" @?= Right [SExp (Const (IntVal 1))],
   testCase "number -1 - IntVal -1" $
     parseString "-1" @?= Right [SExp (Const (IntVal (-1)))],
   testCase "number 10000 - IntVal 10000" $
     parseString "10000" @?= Right [SExp (Const (IntVal 10000))],
   testCase "number -10000 - IntVal -10000" $
     parseString "-10000" @?= Right [SExp (Const (IntVal (-10000)))],
   testCase "number '-45.0' - Error'" $
     parseString "-45.0" @?= Left "No valid parse",
   testCase "number  +1 - Error" $
     parseString "+1" @?= Left "No valid parse",
   testCase "number 1 0 - Error" $
     parseString "1 0" @?= Left "No valid parse"]

relOperTests = testGroup "relation Operation Tests"
  [testCase "1>2 - 1 Greater 2" $
     parseString "1>2" @?= Right [SExp (Oper Greater (Const (IntVal 1)) (Const (IntVal 2)))],
   testCase "1<2 - 1 Less 2" $
     parseString "1<2" @?= Right [SExp (Oper Less (Const (IntVal 1)) (Const (IntVal 2)))],
   testCase "1>=2 - 1 Not Less 2" $
     parseString "1>=2" @?= Right [SExp (Not (Oper Less (Const (IntVal 1)) (Const (IntVal 2))))],
   testCase "1<=2 - 1 Not Greater 2" $
     parseString "1<=2" @?= Right [SExp (Not (Oper Greater (Const (IntVal 1)) (Const (IntVal 2))))],
   testCase "1==2 - 1 Eq 2" $
     parseString "1==2"  @?= Right [SExp (Oper Eq (Const (IntVal 1)) (Const (IntVal 2)))],
   testCase "1!=2 - 1 Not Eq 2" $
     parseString "1" @?= Right [SExp (Not (Oper Eq (Const (IntVal 1)) (Const (IntVal 2))))],
   testCase "1 in 2 - 1 In 2" $
     parseString "1 in 2" @?= Right [SExp (Oper In (Const (IntVal 1)) (Const (IntVal 2)))],
   testCase "1 not in 2 - 1 Not In 2" $
     parseString "1 not in 2" @?= Right [SExp (Not (Oper In (Const (IntVal 1)) (Const (IntVal 2))))]]

arithOperTests = testGroup "airthmetic Tests"
  [testCase "arithOper 1+2 - Plus 1 2" $
     parseString "1+2" @?= Right [SExp (Oper Plus (Const (IntVal 1)) (Const (IntVal 2)))],
   testCase "arithOper 1-2 - Minus 1 2" $
     parseString "1-2" @?= Right [SExp (Oper Minus (Const (IntVal 1)) (Const (IntVal 2)))],
   testCase "arithOper 1+2+3 - Plus 1 Plus 2 3" $
     parseString "1+2+3" @?= Right [SExp (Oper Plus (Const (IntVal 1)) (Oper Plus (Const (IntVal 2)) (Const (IntVal 3))))],
   testCase "arithOper 1-2-3 - Minus 1 Minus 2 3" $
     parseString "1-2-3" @?= Right [SExp (Oper Minus (Const (IntVal 1)) (Oper Minus (Const (IntVal 2)) (Const (IntVal 3))))]]

factorOperTests = testGroup "factor Tests"
  [testCase "factorOper 1*2 - Times 1 2" $
     parseString "1*2" @?= Right [SExp (Oper Times (Const (IntVal 1)) (Const (IntVal 2)))],
   testCase "factorOper 1//2 - Div 1 2" $
     parseString "1//2" @?= Right [SExp (Oper Div (Const (IntVal 1)) (Const (IntVal 2)))],
   testCase "factorOper 1%2 - Mod 1 2" $
     parseString "1%2" @?= Right [SExp (Oper Minus (Const (IntVal 1)) (Const (IntVal 2)))],
   testCase "factorOper 1*2*3 - Times 1 Times 2 3" $
     parseString "1*2*3" @?= Right [SExp (Oper Times (Const (IntVal 1)) (Oper Times (Const (IntVal 2)) (Const (IntVal 3))))],
   testCase "factorOper 1//2//3 - Div 1 Div 2 3" $
     parseString "1//2//3" @?= Right [SExp (Oper Div (Const (IntVal 1)) (Oper Div (Const (IntVal 2)) (Const (IntVal 3))))],
   testCase "factorOper 1%2%3 - Mod 1 Mod 2 3" $
     parseString "1%2%3" @?= Right [SExp (Oper Mod (Const (IntVal 1)) (Oper Mod (Const (IntVal 2)) (Const (IntVal 3))))]]

mixOperTests = testGroup "mix operations Tests"
  [testCase "number 0 - IntVal 0" $
     parseString "0" @?= Right [SExp (Const (IntVal 0))],
   testCase "number -0 - IntVal 0" $
     parseString "-0" @?= Right [SExp (Const (IntVal 0))]]

finalExprTests = testGroup "finalExpr Tests"
  [testCase "oper 2+1*2 - Times 1 2" $
     parseString "2+1*2" @?= Right [SExp (Oper Plus (Const (IntVal 2)) (Oper Times (Const (IntVal 1)) (Const (IntVal 2))))],
   testCase "oper 2-1//2 - Div 1 2" $
     parseString "2-1//2" @?= Right [SExp (Oper Minus (Const (IntVal 2)) (Oper Div (Const (IntVal 1)) (Const (IntVal 2))))],
   testCase "oper 2+1%2 - Plus 2 Mod 1 2" $
     parseString "2+1%2" @?= Right [SExp (Oper Plus (Const (IntVal 2)) (Oper Mod (Const (IntVal 1)) (Const (IntVal 2))))],
  --  testCase "oper 2+1*2*3 - Times 1 Times 2 3" $
  --    parseString "2+1*2*3" @?= Right [SExp (Oper Times (Const (IntVal 1)) (Oper Times (Const (IntVal 2)) (Const (IntVal 3))))],
   testCase "oper 1//2-3 - Minus 1 Div 2 3" $
     parseString "1//2-3" @?= Right [SExp (Oper Minus (Const (IntVal 1)) (Oper Div (Const (IntVal 2)) (Const (IntVal 3))))],
   testCase "oper 1%2-3 - Minus 1 Mod 2 3" $
     parseString "1%2-3" @?= Right [SExp (Oper Minus (Const (IntVal 1)) (Oper Mod (Const (IntVal 2)) (Const (IntVal 3))))]]