-- Rudimentary test suite. Feel free to replace anything.

import BoaAST
import BoaParser

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests = testGroup "Minimal tests" [
  testCase "simple success" $
    parseString "2 + two" @?=
      Right [SExp (Oper Plus (Const (IntVal 2)) (Var "two"))],
  testCase "simple failure" $
    -- avoid "expecting" very specific parse-error messages
    case parseString "wow!" of
      Left e -> return ()  -- any message is OK
      Right p -> assertFailure $ "Unexpected parse: " ++ show p]

-- MY TESTS
-- readP_to_S ident "x" ==[(Right (Var "x"),"")] OK
-- readP_to_S ident "_" == [(Right (Var "_"),"")] OK
-- readP_to_S ident "1" == [] OK
-- readP_to_S ident "x1" == [(Right (Var "x1"),"")] OK
-- readP_to_S ident "x_1" == [(Right (Var "x_"),"1"),(Right (Var "x_1"),"")] OK
-- readP_to_S ident "x11" == [(Right (Var "x1"),"1"),(Right (Var "x11"),"")] OK
-- readP_to_S ident "_1" == [(Right (Var "_1"),"")] OK
-- readP_to_S ident "" == [] OK
-- readP_to_S ident "None" == [(Right (Var "No"),"ne"),(Right (Var "Non"),"e"),(Left (Keyword "None"),"")] OK
-- readP_to_S ident "True" == [(Right (Var "Tr"),"ue"),(Right (Var "Tru"),"e"),(Left (Keyword "True"),"")] OK
-- readP_to_S ident "False" == [(Right (Var "Fa"),"lse"),(Right (Var "Fal"),"se"),(Right (Var "Fals"),"e"),(Left (Keyword "False"),"")] OK
-- readP_to_S ident "for" == [(Right (Var "fo"),"r"),(Left (Keyword "for"),"")] OK
-- readP_to_S ident "if" == [(Left (Keyword "if"),"")] OK
-- readP_to_S ident "in" == [(Left (Keyword "in"),"")] OK
-- readP_to_S ident "not" == [(Right (Var "no"),"t"),(Left (Keyword "not"),"")] OK

-- readP_to_S numConst "-5" == [(Const (IntVal 5),"")] OK RETURN TYPE PROBLEM
-- readP_to_S numConst "-05" == [(Const (IntVal 0),"5")] OK
-- readP_to_S numConst "-50000" == [...(Const (IntVal 50000),"")] OK RETURN TYPE PROBLEM
-- readP_to_S numConst "- 50000" == [] OK
-- readP_to_S numConst "+10" == [] OK
-- readP_to_S numConst "4" == [(Const (IntVal 4),"")] OK

-- readP_to_S stringConst "'asdsa'" == [(Const (StringVal "asdsa"),"")] OK
-- readP_to_S stringConst "'asdsa@" == [] OK
-- readP_to_S stringConst "''"  == [(Const (StringVal ""),"")] OK
-- readP_to_S stringConst "'asdss   '" == [...,(Const (StringVal "asdss   "),"")] OK
-- readP_to_S stringConst "'asd\'sa'" == [(Const (StringVal "asd"),"sa'"),(Const (StringVal "asd'sa"),"")] OK
-- readP_to_S stringConst "'asd\nsa'" == [(Const (StringVal "asd\nsa"),"")] OK
-- readP_to_S stringConst "'asd\\sa'" == [(Const (StringVal "asd\\sa"),"")] OK
-- readP_to_S stringConst "'asd//sa'" == [] OK