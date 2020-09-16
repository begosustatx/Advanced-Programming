-- Skeleton test suite using Tasty.
-- Fell free to modify or replace anything in this file

import BoaAST
import BoaInterp

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests :: TestTree
tests = testGroup "Tests" [stubbyTests, unitTests]

stubbyTests = testGroup "Stubby Tests"
  [testCase "crash test" $
    execute [SExp (Call "print" [Oper Plus (Const (IntVal 2))
                                           (Const (IntVal 2))]),
             SExp (Var "hello")]
      @?= (["4"], Just (EBadVar "hello")),
   testCase "execute misc.ast from handout" $
     do pgm <- read <$> readFile "examples/misc.ast"
        out <- readFile "examples/misc.out"
        execute pgm @?= (lines out, Nothing)]

unitTests = testGroup "Unit Tests" [abortTests, lookTests, withBindingTests, outputTests, 
                                   truthyTests, truthy'Tests, operateTests, --convertTests, Until we fix it
                                   applyTests, evalTests, execTests] 

f :: Either RunError () -> Either RunError ()
f x = x

abortTests = testGroup "abort Tests"
  [testCase "abort EBadVar x" $
  runComp (abort (EBadVar "x")) [("x",NoneVal)] @?= (f (Left (EBadVar "x")),[]),
   testCase "abort EBadFun sum" $
    runComp (abort (EBadFun "sum")) [("x",NoneVal)] @?= (f (Left (EBadFun "sum")),[]),
   testCase "abort EBadArg x" $
    runComp (abort (EBadArg "x")) [("x",NoneVal)] @?= (f (Left (EBadArg "x")),[])]

lookTests = testGroup "look Tests"
 [testCase "look x EBadVar env empty" $
    runComp (look "x") [] @?= (Left (EBadVar "x"),[]),
  testCase "look x NoneVal env x" $
    runComp (look "x") [("x",NoneVal)] @?= (Right NoneVal,[]),
  testCase "look x EBadVar env y" $
    runComp (look "x") [("y",NoneVal)] @?= (Left (EBadVar "x"),[]),
  testCase "look x IntVal env x" $
    runComp (look "x") [("x",IntVal 3)] @?= (Right (IntVal 3),[]),
  testCase "look x TrueVal env x" $
    runComp (look "x") [("x",TrueVal)] @?= (Right TrueVal,[]),
  testCase "look x FalseVal env x" $
    runComp (look "x") [("x",FalseVal)] @?= (Right FalseVal,[]),
  testCase "look x NoneVal" $
    runComp (look "x") [("x",StringVal "text")] @?= (Right (StringVal "text"),[]),
  testCase "look x StringVal env x" $
    runComp (look "x") [("x",ListVal [StringVal "text"])] @?= (Right (ListVal [StringVal "text"]),[]),
  testCase "look x TrueVal env x values" $
    runComp (look "x") [("x",TrueVal),("x",FalseVal),("x",IntVal 3),("x",ListVal [StringVal "text"])] @?= (Right TrueVal,[]),
  testCase "look x ListVal [StringVal]" $
    runComp (look "x") [("y",TrueVal),("w",FalseVal),("s",IntVal 3),("x",ListVal [StringVal "text"])] @?= (Right (ListVal [StringVal "text"]),[]),
  testCase "look x EBadVar env list of not x vars" $
    runComp (look "x") [("y",TrueVal),("w",FalseVal),("s",IntVal 3),("x1",ListVal [StringVal "text"])] @?= (Left (EBadVar "x"),[])]

withBindingTests = testGroup "withBinding Tests"
  [testCase "withBinding NoneVal x" $
    runComp (withBinding "x" NoneVal (look "x")) [] @?= (Right NoneVal,[]),
  testCase "withBinding IntVal x" $
    runComp (withBinding "x" (IntVal 3) (look "x")) [] @?= (Right (IntVal 3),[]),
  testCase "withBinding TrueVal x" $
    runComp (withBinding "x" (TrueVal) (look "x")) [] @?= (Right TrueVal,[]),
  testCase "withBinding FalseVal x" $
    runComp (withBinding "x" (FalseVal) (look "x")) [] @?= (Right FalseVal,[]),
  testCase "withBinding StringVal x" $
    runComp (withBinding "x" (StringVal "text") (look "x")) [] @?= (Right (StringVal "text"),[]),
  testCase "withBinding ListVal StringVal x" $
    runComp (withBinding "x" (ListVal [StringVal "text"]) (look "x")) [] @?= (Right (ListVal [StringVal "text"]),[]),
  testCase "withBinding ListVal Vals x" $
    runComp (withBinding "x" (ListVal [StringVal "text",TrueVal, FalseVal]) (look "x")) [] @?= (Right (ListVal [StringVal "text",TrueVal, FalseVal]),[])]

outputTests = testGroup "output Tests"
  [testCase "output x" $
    runComp (output "x") [] @?= (Right (),["x"]),
  testCase "output empty string" $
    runComp (output "") [] @?= (Right (),[""]),
  testCase "output some text" $
    runComp (output "some text") [] @?= (Right (),["some text"]),
  testCase "output x" $
    runComp (output "x") [("x",NoneVal)] @?= (Right (),["x"])]

truthyTests = testGroup "Truthy Tests"
  [testCase "truthy FalseVal" $
    truthy FalseVal @?= False,
  testCase "truthy (IntVal 0)" $
    truthy (IntVal 0) @?= False,
  testCase "truthy (StringVal Empty)" $
    truthy (StringVal "") @?= False,
  testCase "truthy (ListVal [])" $
    truthy (ListVal []) @?= False,
  testCase "truthy NoneVal" $
    truthy NoneVal @?= False,
  testCase "truthy TrueVal" $
    truthy TrueVal @?= True,
  testCase "truthy (IntVal 1)" $
    truthy (IntVal 1) @?= True,
  testCase "truthy (StringVal \"text\")" $
    truthy (StringVal "text") @?= True,
  testCase "truthy (ListVal [NoneVal])" $
    truthy (ListVal [NoneVal]) @?= True]

truthy'Tests = testGroup "Truthy' Tests"
  [testCase "truthy' FalseVal" $
    truthy FalseVal @?= False,
  testCase "truthy' TrueVal" $
    truthy TrueVal @?= True]

operateTests = testGroup "Operate Tests"
  [testCase "operate Plus" $
    operate Plus (IntVal 0) (IntVal 0) @?= Right (IntVal 0),
  testCase "operate Minus" $
    operate Minus (IntVal 0) (IntVal 0) @?= Right (IntVal 0),
  testCase "operate Times" $
    operate Times (IntVal 0) (IntVal 0) @?= Right (IntVal 0),
  testCase "operate Div by Zero" $
    operate Div (IntVal 0) (IntVal 0) @?= Left "Divide by Zero",
  testCase "operate Mod by Zero" $
    operate Mod (IntVal 0) (IntVal 0) @?= Left "Modulo by Zero",
  testCase "operate Eq 0@?=0" $
    operate Eq (IntVal 0) (IntVal 0) @?= Right TrueVal,
  testCase "operate Less 0<0" $
    operate Less (IntVal 0) (IntVal 0) @?= Right FalseVal,
  testCase "operate Greater 0>0" $
    operate Greater (IntVal 0) (IntVal 0) @?= Right FalseVal,
  testCase "operate In by []" $
    operate In (IntVal 0) (ListVal []) @?= Right FalseVal,
  testCase "operate Plus by IntVal" $
    operate Plus (IntVal 85) (IntVal 45) @?= Right (IntVal 130),
  testCase "operate Minus Negative" $
    operate Minus (IntVal 0) (IntVal 45) @?= Right (IntVal (-45)),
  testCase "operate Times by 100" $
    operate Times (IntVal 1) (IntVal 100) @?= Right (IntVal 100),
  testCase "operate Div by 5" $
    operate Div (IntVal 0) (IntVal 5) @?= Right (IntVal 0),
  testCase "operate Mod by 5" $
    operate Mod (IntVal 0) (IntVal 5) @?= Right (IntVal 0),
  testCase "operate Eq 0@?=5" $
    operate Eq (IntVal 0) (IntVal 5) @?= Right FalseVal,
  testCase "operate Eq by Different Values" $
    operate Eq (IntVal 0) (TrueVal) @?= Right FalseVal,
  testCase "operate Eq different StringVal" $
    operate Eq (StringVal "nothing") (StringVal "something") @?= Right FalseVal,
  testCase "operate Eq StringVal" $
    operate Eq (StringVal "something") (StringVal "something") @?= Right TrueVal,
  testCase "operate Eq []@?=5" $
    operate Eq (ListVal [IntVal 0]) (IntVal 5) @?= Right FalseVal,
  testCase "operate Less 5<0" $
    operate Less (IntVal 5) (IntVal 0) @?= Right FalseVal,
  testCase "operate Greater 5>0" $
    operate Greater (IntVal 5) (IntVal 0) @?= Right TrueVal,
  testCase "operate In 0 by [0]" $
    operate In (IntVal 0) (ListVal [IntVal 0]) @?= Right TrueVal,
  testCase "operate Plus by StringVal" $
    operate Plus (StringVal "") (IntVal 0) @?= Left "The values must be proper for chosen operator",
  testCase "operate Minus by NoneVal" $
    operate Minus (NoneVal) (NoneVal) @?= Left "The values must be proper for chosen operator",
  testCase "operate Times by StringVal" $
    operate Times (StringVal "") (StringVal "") @?= Left "The values must be proper for chosen operator",
  testCase "operate Div by NoneVal" $
    operate Div (IntVal 0) (NoneVal) @?= Left "The values must be proper for chosen operator",
  testCase "operate Mod by StringVal" $
    operate Mod (IntVal 0) (StringVal "") @?= Left "The values must be proper for chosen operator",
  testCase "operate Eq Empty ListVal" $
    operate Eq (ListVal []) (ListVal []) @?= Right TrueVal,
  testCase "operate Less StringVal IntVal" $
    operate Less (StringVal "") (IntVal 0) @?= Left "The values must be proper for chosen operator",
  testCase "operate Greater IntVal and StringVal" $
    operate Greater (IntVal 0) (StringVal "") @?= Left "The values must be proper for chosen operator",
  testCase "operate In IntVal and StringVal" $
    operate In (IntVal 0) (StringVal "") @?= Left "The values must be proper for chosen operator"]

{- Until we fix it
convertTests = testGroup "ConvertToString Tests"
 [testCase "convertToString NoneVal" $
    convertToString NoneVal @?= "None",
  testCase "convertToString TrueVal" $
    convertToString TrueVal @?= "True",
  testCase "convertToString FalseVal" $
    convertToString FalseVal @?= "False",
  testCase "convertToString IntVal 0" $
    convertToString (IntVal 0) @?= "0",
  testCase "convertToString IntVal 100000" $
    convertToString (IntVal 100000) @?= "100000",
  testCase "convertToString IntVal -40" $
    convertToString (IntVal (-40)) @?= "-40",
  testCase "convertToString Empty StringVal" $
    convertToString (StringVal "") @?= "",
  testCase "convertToString StringVal text" $
    convertToString (StringVal "text") @?= "text",
  testCase "convertToString StringVal multiple text" $
    convertToString (StringVal "text text 2") @?= "text text 2",
  testCase "convertToString ListVal [NoneVal]" $
    convertToString (ListVal [NoneVal]) @?= "[None]",
  testCase "convertToString ListVal [FalseVal]" $
    convertToString (ListVal [FalseVal]) @?= "[False]",
  testCase "convertToString ListVal [TrueVal]" $
    convertToString (ListVal [TrueVal]) @?= "[True]",
  testCase "convertToString ListVal [[]]" $
    convertToString (ListVal [ListVal []]) @?= "[[]]"]
-}
applyTests = testGroup "apply Tests"
 [testCase "apply range 10" $
    runComp (apply "range" [(IntVal 10)]) [] @?= (Right (ListVal [IntVal 0,IntVal 1,IntVal 2,IntVal 3,IntVal 4,IntVal 5,IntVal 6,IntVal 7,IntVal 8,IntVal 9]),[]),
  testCase "apply range 1,10" $
    runComp (apply "range" [IntVal 1,IntVal 10]) [] @?= (Right (ListVal [IntVal 1,IntVal 2,IntVal 3,IntVal 4,IntVal 5,IntVal 6,IntVal 7,IntVal 8,IntVal 9]),[]),
  testCase "apply range 1,10,4" $
    runComp (apply "range" [IntVal 1,IntVal 10,IntVal 4]) [] @?= (Right (ListVal [IntVal 1,IntVal 5,IntVal 9]),[]),
  testCase "apply range 1,10,-4" $
    runComp (apply "range" [IntVal 1,IntVal 10,IntVal (-4)]) [] @?= (Right (ListVal []),[]),
  testCase "apply range 10,1,-3" $
    runComp (apply "range" [IntVal 10,IntVal 1,IntVal (-3)]) [] @?= (Right (ListVal [IntVal 10,IntVal 7,IntVal 4]),[]),
  testCase "apply range 10,1,-4" $
    runComp (apply "range" [IntVal 10,IntVal 1,IntVal (-4)]) [] @?= (Right (ListVal [IntVal 10,IntVal 6,IntVal 2]),[]),
  testCase "apply range 10,1" $
    runComp (apply "range" [IntVal 10,IntVal 1]) [] @?= (Right (ListVal []),[]),
  testCase "apply range -10" $
    runComp (apply "range" [IntVal (-10)]) [] @?= (Right (ListVal []),[]),
  testCase "apply range NoneVal" $
    runComp (eval (Call "range" [Const NoneVal])) [] @?= (Left (EBadArg "[NoneVal]"),[]),
  testCase "apply range []" $
    runComp (eval (Call "range" [])) []  @?= (Left (EBadArg "[]"),[]),
  testCase "apply foobar []" $
    runComp (eval (Call "foobar" [])) [] @?= (Left (EBadFun "foobar"),[]),
  testCase "apply print []" $
    runComp (apply "print" []) [] @?= (Right NoneVal,[""]),
  testCase "apply print [3]" $
    runComp (apply "print" [IntVal 3]) [] @?= (Right NoneVal,["3"]),
  testCase "apply print [3,None]" $
    runComp (apply "print" [IntVal 3, NoneVal]) [] @?= (Right NoneVal,["3 None"]),
  testCase "apply print [3,text,[]]" $
    runComp (apply "print" [IntVal 3, StringVal "text", ListVal []]) [] @?= (Right NoneVal,["3 text []"]),
  testCase "apply print [3,text,[None,None]]" $
    runComp (apply "print" [IntVal 3, StringVal "text", ListVal [NoneVal, NoneVal]]) [] @?= (Right NoneVal,["3 text [None, None]"]),
  testCase "apply print [3,text,[[]]]" $
    runComp (apply "print" [IntVal 3, StringVal "text", ListVal [ListVal []]]) [] @?= (Right NoneVal,["3 text [[]]"])]

evalTests = testGroup "eval Tests"
 [testCase "eval (3+4+3)" $
    runComp (eval (Oper Plus (Oper Plus (Const (IntVal 3)) (Const (IntVal 4))) (Const (IntVal 3)))) [] @?= (Right (IntVal 10),[]),
  testCase "eval not x (x not in env)" $
    runComp (eval (Not (Var "x"))) [] @?= (Left (EBadVar "x"),[]),
  testCase "eval not 0" $
    runComp (eval (Not (Const (IntVal 0)))) [] @?= (Right TrueVal,[]),
  testCase "exec not 45" $
    runComp (eval (Not (Const (IntVal 45)))) [] @?= (Right FalseVal,[]),
  testCase "exec not empty" $
    runComp (eval (Not (Const (StringVal "")))) [] @?= (Right TrueVal,[]),
  testCase "exec not text" $
    runComp (eval (Not (Const (StringVal "text")))) [] @?= (Right FalseVal,[]),
  testCase "exec not NoneVal" $
    runComp (eval (Not (Const NoneVal))) [] @?= (Right TrueVal,[]),
  testCase "exec Not x Trueval in env" $
    runComp (eval (Not (Var "x"))) [("x",TrueVal)] @?= (Right TrueVal,[]) ,
  testCase "exec Not x Trueval in env" $
    runComp (eval (Not (List []))) [] @?= (Right TrueVal,[]),
  testCase "eval Call range(1,x) x unbound" $
    runComp (eval (Call "range" [Const (IntVal 1), Var "x"])) [] @?= (Left (EBadVar "x"),[]),
  testCase "eval Call range(1,x) x bound 5" $
    runComp (eval (Call "range" [Const (IntVal 1), Var "x"])) [("x",IntVal 5)] @?= (Right (ListVal [IntVal 1,IntVal 2,IntVal 3,IntVal 4]),[]),
  testCase "eval Call range(5)" $
    runComp (eval (Call "range" [Const (IntVal 5)])) [] @?= (Right (ListVal [IntVal 0,IntVal 1,IntVal 2,IntVal 3,IntVal 4]),[]),
  testCase "eval Call range(1,10,2)" $
    runComp (eval (Call "range" [Const (IntVal 1), Const (IntVal 10), Const (IntVal 2)])) [] @?= (Right (ListVal [IntVal 1,IntVal 3,IntVal 5,IntVal 7,IntVal 9]),[]),
  testCase "eval Call range(1,10)" $
    runComp (eval (Call "range" [Const (IntVal 1), Const (IntVal 10)])) [] @?= (Right (ListVal [IntVal 1,IntVal 2,IntVal 3,IntVal 4,IntVal 5,IntVal 6,IntVal 7,IntVal 8,IntVal 9]),[]),
  testCase "eval List (NoneVal, 3)" $
    runComp (eval (List [Const NoneVal, Const (IntVal 3)])) [] @?=(Right (ListVal [NoneVal,IntVal 3]),[]),
  testCase "eval List (NoneVal)" $
    runComp (eval (List [Const NoneVal])) [] @?= (Right (ListVal [NoneVal]),[]),
  testCase "eval List []" $
    runComp (eval (List [])) [] @?= (Right (ListVal []),[]),
  testCase "eval List (3,2,NoneVal, string)" $
    runComp (eval (List [Const (IntVal 3),Const (IntVal 2), Const NoneVal,Const (StringVal "2")])) [] @?= (Right (ListVal [IntVal 3,IntVal 2,NoneVal,StringVal "2"]),[]),
  testCase "exec Compr x" $
    runComp (eval (Compr (Var "x") [])) [("x",IntVal 4)] @?= (Right (IntVal 4),[]),
  testCase "exec Compr EBadVar" $
    runComp (eval (Compr (Var "x") [])) [] @?= (Left (EBadVar "x"),[]),
  testCase "exec Compr (x*x) for range(10)" $
    runComp (eval (Compr (Oper Times (Var "x") (Var "x")) [CCFor "x" (Call "range" [Const (IntVal 10)])])) [("x",IntVal 4)] @?= (Right (IntVal 321),[]),
  testCase "exec Compr (x) if x env x 4" $
    runComp (eval (Compr (Var "x") [CCIf (Var "x")])) [("x",IntVal 4)] @?= (Right (ListVal [IntVal 4]),[]),
  testCase "eval Compr (x+1) for range (2,5)" $
    runComp (eval (Compr (Oper Plus (Var "x") (Const (IntVal 1))) [CCFor "x" (Call "range" [Const (IntVal 2),Const (IntVal 5)])])) [("x",IntVal 4)] @?= (Right (ListVal [IntVal 12]),[])]

execTests = testGroup "exec Tests"
 [testCase "exec []" $
    runComp (exec []) [] @?= (Right (),[]),
  testCase "exec [print(x)]" $
    runComp (exec [SDef "x" (Call "print" [(Var "x")])]) [("x",IntVal 4)] @?= (Right (),["4"])]      

--executeTests = testGroup "execute Tests"