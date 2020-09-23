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
                                    commentTests, clauseTests, callTests, listTests,
                                    associationTest, stmtTests, stmtsTests, 
                                    crashBoaTests, miscBoaTests] 

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
     parseString "'\"#~ '" @?= Right [SExp (Const (StringVal "\"#~ "))],
   testCase "string 'a\\'b\\\\c\\nd' - StringVal 'a'b\\c\nd'" $
     parseString "'a\\'b\\\\c\\nd'"  @?= Right [SExp (Const (StringVal "a'b\\c\nd"))],
   testCase "string 'a\\\n b\\n\\\nc\\\n\\nd' - StringVal 'a'b\\c\nd'" $
    parseString "'a\\\n b\\n\\\nc\\\n\\nd'"  @?= Right [SExp (Const (StringVal "a b\nc\nd"))]]

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
     parseString "1!=2" @?= Right [SExp (Not (Oper Eq (Const (IntVal 1)) (Const (IntVal 2))))],
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

commentTests = testGroup "spaceComment Tests"
  [testCase "No space for not" $
     parseString "[(x)not\tin(not(y)),[(x)for\ty\tin[z]if(u)]]" @?= Right [SExp (List [Not (Oper In (Var "x") (Not (Var "y"))),Compr (Var "x") [CCFor "y" (List [Var "z"]),CCIf (Var "u")]])],
   testCase "Not with comments between" $
     parseString "not#foo\nx" @?= Right [SExp (Not (Var "x"))],
   testCase "Many comments with newline" $
     parseString "# \n# \n# \n# \n# \n# \n# \n# \n# \n# \nx# \n# \n# \n# \n# \n# \n# \n# \n# \n# \n" @?= Right [SExp (Var "x")]]

clauseTests = testGroup "clause Tests"
  [testCase "for cluase y in z" $
     parseString "[x for y in z]" @?= Right [SExp (Compr (Var "x") [CCFor "y" (Var "z")])],
   testCase "for cluase y in (10+3)" $
     parseString "[x for y in (10+3)]" @?= Right [SExp (Compr (Var "x") [CCFor "y" (Oper Plus (Const (IntVal 10)) (Const (IntVal 3)))])],
   testCase "for cluase in if for list" $
     parseString "[x for y in z if u for a in []]" @?= Right [SExp (Compr (Var "x") [CCFor "y" (Var "z"),CCIf (Var "u"),CCFor "a" (List [])])],
   testCase "for clause with possible not keywoard" $
     parseString "[x for ynot in z]" @?= Right [SExp (Compr (Var "x") [CCFor "ynot" (Var "z")])],
   testCase "for clause with additional in operator" $
     parseString "[x for y in z in u]" @?= Right [SExp (Compr (Var "x") [CCFor "y" (Oper In (Var "z") (Var "u"))])]]

callTests = testGroup "callTests Tests"
  [testCase "Call of empty" $
     parseString "f()" @?= Right [SExp (Call "f" [])],
   testCase "Call of a list of False Value" $
     parseString "f([False])" @?= Right [SExp (Call "f" [List [Const FalseVal]])],
   testCase "Call of a list expression 2+y" $
     parseString "f([2+y])" @?= Right [SExp (Call "f" [List [Oper Plus (Const (IntVal 2)) (Var "y")]])],
   testCase "Call of a list not expression and list" $
     parseString "f([not (z),[u]])" @?= Right [SExp (Call "f" [List [Not (Var "z"),List [Var "u"]]])],
   testCase "Call of a list of values" $
     parseString "f([False,2+y,not(z),[u]])" @?= Right [SExp (Call "f" [List [Const FalseVal,Oper Plus (Const (IntVal 2)) (Var "y"),Not (Var "z"),List [Var "u"]]])]]

listTests = testGroup "list Tests"
  [testCase "empty list" $
     parseString "[]" @?= Right [SExp (List [])],
   testCase "list of empty list" $
     parseString "[[]]" @?= Right [SExp (List [List []])],
   testCase "list of var" $
     parseString "[x]" @?= Right [SExp (Compr (Var "x") [CCFor "y" (Oper Plus (Const (IntVal 10)) (Const (IntVal 3)))])],
   testCase "list of number" $
     parseString "[23]" @?= Right [SExp (List [Const (IntVal 23)])],
   testCase "list of operation" $
     parseString "[234+(-45)]" @?= Right [SExp (List [Oper Plus (Const (IntVal 234)) (Const (IntVal (-45)))])],
   testCase "list of elements" $
     parseString "[23,342,234]" @?= Right [SExp (List [Const (IntVal 23),Const (IntVal 342),Const (IntVal 234)])]]

associationTest = testGroup "associationTest Tests"
  [testCase "oper 2+1*2 - Times 1 2" $
     parseString "2+1*2" @?= Right [SExp (Oper Plus (Const (IntVal 2)) (Oper Times (Const (IntVal 1)) (Const (IntVal 2))))],
   testCase "oper 2-1//2 - Div 1 2" $
     parseString "2-1//2" @?= Right [SExp (Oper Minus (Const (IntVal 2)) (Oper Div (Const (IntVal 1)) (Const (IntVal 2))))],
   testCase "oper 2+1%2 - Plus 2 Mod 1 2" $
     parseString "2+1%2" @?= Right [SExp (Oper Plus (Const (IntVal 2)) (Oper Mod (Const (IntVal 1)) (Const (IntVal 2))))],
   testCase "oper x%y-z>(not u) - Greate Mox x y Not u" $
     parseString "x%y-z>(not u)" @?= Right [SExp (Oper Greater (Oper Minus (Oper Mod (Var "x") (Var "y")) (Var "z")) (Not (Var "u")))],
   testCase "oper x*y//z%u - Div Mul x y Mod z u" $
     parseString "x*y//z%u" @?= Right [SExp (Oper Mod (Oper Div (Oper Times (Var "x") (Var "y")) (Var "z")) (Var "u"))],
   testCase "oper 1//2-3 - Minus 1 Div 2 3" $
     parseString "1//2-3" @?= Right [SExp (Oper Minus (Const (IntVal 1)) (Oper Div (Const (IntVal 2)) (Const (IntVal 3))))],
   testCase "oper 1%2-3 - Minus 1 Mod 2 3" $
     parseString "1%2-3" @?= Right [SExp (Oper Minus (Const (IntVal 1)) (Oper Mod (Const (IntVal 2)) (Const (IntVal 3))))]]

stmtTests = testGroup "statment Tests"
  [testCase "definition of x error parse" $
     parseString "x=" @?= Left "No valid parse",
   testCase "definition of x being number" $
     parseString "x=3" @?= Right [SDef "x" (Const (IntVal 3))],
   testCase "definition of x being an operation expression" $
     parseString "x=1%2" @?= Right [SDef "x" (Oper Mod (Const (IntVal 1)) (Const (IntVal 2)))],
   testCase "definition of x being an for clause" $
     parseString "x=[i for x in 2]" @?= Right [SDef "x" (Compr (Var "i") [CCFor "x" (Const (IntVal 2))])],
   testCase "definition of x being list" $
     parseString "x=[1,2,3,4,5,6,7,8,9,10]" @?= Right [SDef "x" (List [Const(IntVal (i+1)) | i <- [0..9]])],
   testCase "definition of x being list" $
     parseString "x=[1,2,3,4,5,6,7,8,9,10]" @?= Right [SDef "x" (List [Const(IntVal (i+1)) | i <- [0..9]])],
   testCase "definition of x being not True" $
     parseString "x=not True" @?= Right [SDef "x" (Not (Const TrueVal))]]

stmtsTests = testGroup "statments Tests"
  [testCase "empty statments just ;" $
     parseString ";" @?=Left "No valid parse",
   testCase "definition statments" $
     parseString "x=3;y=2" @?= Right [SDef "x" (Const (IntVal 3)),SDef "y" (Const (IntVal 2))],
   testCase "expression statments" $
     parseString "3==3;1-2" @?= Right [SExp (Oper Eq (Const (IntVal 3)) (Const (IntVal 3))),SExp (Oper Minus (Const (IntVal 1)) (Const (IntVal 2)))],
   testCase "definitions and expression statments" $
     parseString "3+3;x=3;y=2" @?= Right [SExp (Oper Plus (Const (IntVal 3)) (Const (IntVal 3))),SDef "x" (Const (IntVal 3)),SDef "y" (Const (IntVal 2))],
   testCase "definition statments" $
     parseString "x=3;[1,2,3,4]" @?= Right [SDef "x" (Const (IntVal 3)),SExp (List [Const(IntVal (i+1)) | i <- [0..3]])]]

crashBoaTests = testGroup "parse crash.boa from handout Tests"
  [testCase "first line of misc.boa parse" $
     parseString "print(2+2); hello" @?= Right [SExp (Call "print" [Oper Plus (Const (IntVal 2)) (Const (IntVal 2))]), SExp (Var "hello")]]

miscBoaTests = testGroup "parse misc.boa from handout Tests"
  [testCase "first line of misc.boa parse" $
     parseString "squares = [x*x for x in range(10)]" @?= Right [SDef "squares" (Compr (Oper Times (Var "x") (Var "x")) [CCFor "x" (Call "range" [Const (IntVal 10)])])],
   testCase "second line of misc.boa parse" $
     parseString "print([123, [squares, print(321)]])" @?= Right [SExp (Call "print" [List [Const (IntVal 123), List [Var "squares", Call "print" [Const (IntVal 321)]]]])] ,
   testCase "third line of misc.boa parse" $
     parseString "print('Odd squares:', [x for x in squares if x % 2 == 1])" @?= Right [SExp (Call "print" [Const (StringVal "Odd squares:"), Compr (Var "x") [CCFor "x" (Var "squares"), CCIf (Oper Eq (Oper Mod (Var "x") (Const (IntVal 2))) (Const (IntVal 1)))]])],
   testCase "fourth line of misc.boa parse" $
     parseString "n = 5" @?= Right [SDef "n" (Const (IntVal 5))],
   testCase "fifth line of misc.boa parse" $
     parseString "composites = [j for i in range(2, n) for j in range(i*2, n*n, i)]" @?= Right [SDef "composites" (Compr (Var "j") [CCFor "i" (Call "range" [Const (IntVal 2),Var "n"]),CCFor "j" (Call "range" [Oper Times (Var "i") (Const (IntVal 2)), Oper Times (Var "n") (Var "n"), Var "i"])])],
   testCase "sixth line of misc.boa parse" $
     parseString "print('Printing all primes below', n*n)" @?= Right [SExp (Call "print" [Const (StringVal "Printing all primes below"), Oper Times (Var "n") (Var "n")])],
   testCase "seventh line of misc.boa parse" $
     parseString "[print(x) for x in range(2,n*n) if x not in composites]" @?= Right [SExp (Compr (Call "print" [Var "x"]) [CCFor "x" (Call "range" [Const (IntVal 2), Oper Times (Var "n") (Var "n")]), CCIf (Not (Oper In (Var "x") (Var "composites")))])]]