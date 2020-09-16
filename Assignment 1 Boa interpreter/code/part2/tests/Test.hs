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
      @?= (["4"], Just (EBadVar "hello"))]
  --  testCase "execute misc.ast from handout" $
  --    do pgm <- read <$> readFile "examples/misc.ast"
  --       out <- readFile "examples/misc.out"
  --       execute pgm @?= (lines out, Nothing)]

unitTests = testGroup "Unit Tests" [truthyTests, truthy'Tests, operateTests]--, convertToStringTests]

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
  testCase "operate Eq 0==0" $
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
  testCase "operate Eq 0==5" $
    operate Eq (IntVal 0) (IntVal 5) @?= Right FalseVal,
  testCase "operate Eq by Different Values" $
    operate Eq (IntVal 0) (TrueVal) @?= Right FalseVal,
  testCase "operate Eq different StringVal" $
    operate Eq (StringVal "nothing") (StringVal "something") @?= Right FalseVal,
  testCase "operate Eq StringVal" $
    operate Eq (StringVal "something") (StringVal "something") @?= Right TrueVal,
  testCase "operate Eq []==5" $
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

{-convertToStringTests = "ConvertToString Tests"
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
    convertToString (ListVal [ListVal []]) @?= "[[]]"]-}

{-
-- Helping code
env = [("x",NoneVal)]
x = undefined -- Comp a :: type
runComp x env

-- abort function test
runComp (abort (EBadVar "x")) [("x",NoneVal)] == (Left (EBadVar "x"),[])
runComp (abort (EBadFun "sum")) [("x",NoneVal)] == (Left (EBadFun "sum"),[])
runComp (abort (EBadArg "x")) [("x",NoneVal)] == (Left (EBadArg "x"),[])

-- look function test
runComp (look "x") [] == (Left (EBadVar "x"),[])
runComp (look "x") [("x",NoneVal)] == (Right NoneVal,[])
runComp (look "x") [("y",NoneVal)] == (Left (EBadVar "x"),[])
runComp (look "x") [("x",IntVal 3)] == (Right (IntVal 3),[])
runComp (look "x") [("x",TrueVal)] == (Right TrueVal,[])
runComp (look "x") [("x",FalseVal)] == (Right FalseVal,[])
runComp (look "x") [("x",StringVal "text")] == (Right (StringVal "text"),[])
runComp (look "x") [("x",ListVal [StringVal "text"])] == (Right (ListVal [StringVal "text"]),[])
runComp (look "x") [("x",TrueVal),("x",FalseVal),("x",IntVal 3),("x",ListVal [StringVal "text"])] == (Right TrueVal,[])
runComp (look "x") [("y",TrueVal),("w",FalseVal),("s",IntVal 3),("x",ListVal [StringVal "text"])] == (Right (ListVal [StringVal "text"]),[])
runComp (look "x") [("y",TrueVal),("w",FalseVal),("s",IntVal 3),("x1",ListVal [StringVal "text"])] == (Left (EBadVar "x"),[])

--withBinding function test
runComp (withBinding "x" NoneVal (look "x")) [] == (Right NoneVal,[])
runComp (withBinding "x" (IntVal 3) (look "x")) [] == (Right (IntVal 3),[])
runComp (withBinding "x" (TrueVal) (look "x")) [] == (Right TrueVal,[])
runComp (withBinding "x" (FalseVal) (look "x")) [] == (Right FalseVal,[])
runComp (withBinding "x" (StringVal "text") (look "x")) [] == (Right (StringVal "text"),[])
runComp (withBinding "x" (ListVal [StringVal "text"]) (look "x")) [] == (Right (ListVal [StringVal "text"]),[])
runComp (withBinding "x" (ListVal [StringVal "text",TrueVal, FalseVal]) (look "x")) [] == (Right (ListVal [StringVal "text",TrueVal, FalseVal]),[])

--output function test
runComp (output "x") [] == (Right (),["x"])
runComp (output "") [] == (Right (),[""])
runComp (output "some text") [] == (Right (),["some text"])
runComp (output "x") [("x",NoneVal)] == (Right (),["x"])

--truthy
truthy TrueVal == True
truthy FalseVal == False
truthy (IntVal 1) == True
truthy (IntVal 0) == False
truthy (StringVal "text") == True
truthy (StringVal "") == False
truthy (ListVal [NoneVal]) == True
truthy (ListVal []) == False
truthy NoneVal == False

--truthy'
truthy' True == TrueVal
truthy' False == FalseVal

--operate
operate Plus (IntVal 0) (IntVal 0) == Right (IntVal 0)
operate Minus (IntVal 0) (IntVal 0) == Right (IntVal 0)
operate Times (IntVal 0) (IntVal 0) == Right (IntVal 0)
operate Div (IntVal 0) (IntVal 0) == Left "Divide by Zero"
operate Mod (IntVal 0) (IntVal 0) == Left "Modulo by Zero"
operate Eq (IntVal 0) (IntVal 0) == Right TrueVal
operate Less (IntVal 0) (IntVal 0) == Right FalseVal
operate Greater (IntVal 0) (IntVal 0) == Right FalseVal
operate In (IntVal 0) (ListVal []) == Right FalseVal
operate Plus (IntVal 85) (IntVal 45) == Right (IntVal 130)
operate Minus (IntVal 0) (IntVal 45) == Right (IntVal (-45))
operate Times (IntVal 1) (IntVal 100) == Right (IntVal 100)
operate Div (IntVal 0) (IntVal 5) == Right (IntVal 0)
operate Mod (IntVal 0) (IntVal 5) == Right (IntVal 0)
operate Eq (IntVal 0) (IntVal 5) == Right FalseVal
operate Eq (IntVal 0) (TrueVal) == Right FalseVal
operate Eq (StringVal "nothing") (StringVal "something") == Right FalseVal
operate Eq (StringVal "something") (StringVal "something") == Right TrueVal
operate Eq (ListVal [IntVal 0]) (IntVal 5) == Right FalseVal
operate Less (IntVal 5) (IntVal 0) == Right FalseVal
operate Greater (IntVal 5) (IntVal 0) == Right TrueVal
operate In (IntVal 0) (ListVal [IntVal 0]) == Right TrueVal
operate Plus (StringVal "") (IntVal 0) == Left "The values must be proper for chosen operator"
operate Minus (NoneVal) (NoneVal) == Left "The values must be proper for chosen operator"
operate Times (StringVal "") (StringVal "") == Left "The values must be proper for chosen operator"
operate Div (IntVal 0) (NoneVal) == Left "The values must be proper for chosen operator"
operate Mod (IntVal 0) (StringVal "") == Left "The values must be proper for chosen operator"
operate Eq (ListVal []) (ListVal []) == Right TrueVal
operate Less (StringVal "") (IntVal 0) == Left "The values must be proper for chosen operator"
operate Greater (IntVal 0) (StringVal "") == Left "The values must be proper for chosen operator"
operate In (IntVal 0) (StringVal "") == Left "The values must be proper for chosen operator"

convertToString (ListVal [NoneVal, TrueVal, FalseVal, IntVal 30, StringVal "something"]) == [None,True,False,30,something]"

-- apply range
runComp (apply "range" [(IntVal 10)]) [] == (Right (ListVal [IntVal 0,IntVal 1,IntVal 2,IntVal 3,IntVal 4,IntVal 5,IntVal 6,IntVal 7,IntVal 8,IntVal 9]),[])
runComp (apply "range" [IntVal 1,IntVal 10]) [] == (Right (ListVal [IntVal 1,IntVal 2,IntVal 3,IntVal 4,IntVal 5,IntVal 6,IntVal 7,IntVal 8,IntVal 9]),[])
runComp (apply "range" [IntVal 1,IntVal 10,IntVal 4]) [] == (Right (ListVal [IntVal 1,IntVal 5,IntVal 9]),[])
runComp (apply "range" [IntVal 1,IntVal 10,IntVal (-4)]) [] == (Right (ListVal []),[])
runComp (apply "range" [IntVal 10,IntVal 1,IntVal (-3)]) [] == (Right (ListVal [IntVal 10,IntVal 7,IntVal 4]),[])
runComp (apply "range" [IntVal 10,IntVal 1,IntVal (-4)]) [] == (Right (ListVal [IntVal 10,IntVal 6,IntVal 2]),[])
runComp (apply "range" [IntVal 10,IntVal 1]) [] == (Right (ListVal []),[])
runComp (apply "range" [IntVal (-10)]) [] == (Right (ListVal []),[])
runComp (eval (Call "range" [Const NoneVal])) [] == (Left (EBadArg "[NoneVal]"),[])
runComp (eval (Call "foobar" [])) [] == (Left (EBadFun "foobar"),[])
runComp (eval (Call "range" [])) []  == (Left (EBadArg "[]"),[])

--apply print
runComp (apply "print" []) [] == (Right NoneVal,[""])
runComp (apply "print" [IntVal 3]) [] == (Right NoneVal,["3"])
runComp (apply "print" [IntVal 3, NoneVal]) [] == (Right NoneVal,["3 None"])
runComp (apply "print" [IntVal 3, StringVal "somesad", ListVal []]) [] == (Right NoneVal,["3 somesad []"])
runComp (apply "print" [IntVal 3, StringVal "somesad", ListVal [NoneVal, NoneVal]]) [] == (Right NoneVal,["3 somesad [None,None]"])
runComp (apply "print" [IntVal 3, StringVal "somesad", ListVal [ListVal []]]) [] == (Right NoneVal,["3 somesad [[]]"])

-- eval
runComp (eval (Oper Plus (Oper Plus (Const (IntVal 3)) (Const (IntVal 4))) (Const (IntVal 3)))) [] == (Right (IntVal 10),[])

-- eval Not
runComp (eval (Not (Var "x"))) [] == (Left (EBadVar "x"),[])
runComp (eval (Not (Const (IntVal 0)))) [] == (Right TrueVal,[])
runComp (eval (Not (Const (IntVal 45)))) [] == (Right FalseVal,[])
runComp (eval (Not (Const (StringVal "")))) [] == (Right TrueVal,[])
runComp (eval (Not (Const (StringVal "asd")))) [] == (Right FalseVal,[])
runComp (eval (Not (Const NoneVal))) [] == (Right TrueVal,[])
runComp (eval (Not (Var "x"))) [] == (Left (EBadVar "x"),[]) 
-- runComp (eval (Not (ListVal []))) [] == ERROR

-- eval Call
runComp (eval (Call "range" [Const (IntVal 1), Var "x"])) [] == (Left (EBadVar "x"),[])
runComp (eval (Call "range" [Const (IntVal 1), Var "x"])) [("x",IntVal 5)] == (Right (ListVal [IntVal 1,IntVal 2,IntVal 3,IntVal 4]),[])
runComp (eval (Call "range" [Const (IntVal 5)])) [] == (Right (ListVal [IntVal 0,IntVal 1,IntVal 2,IntVal 3,IntVal 4]),[])
runComp (eval (Call "range" [Const (IntVal 1), Const (IntVal 10), Const (IntVal 2)])) [] == (Right (ListVal [IntVal 1,IntVal 3,IntVal 5,IntVal 7,IntVal 9]),[])
runComp (eval (Call "range" [Const (IntVal 1), Const (IntVal 10)])) [] == (Right (ListVal [IntVal 1,IntVal 2,IntVal 3,IntVal 4,IntVal 5,IntVal 6,IntVal 7,IntVal 8,IntVal 9]),[])
-- eval List
runComp (eval (List [Const NoneVal, Const (IntVal 3)])) [] ==(Right (ListVal [NoneVal,IntVal 3]),[])
runComp (eval (List [Const NoneVal])) [] == (Right (ListVal [NoneVal]),[])
runComp (eval (List [])) [] == (Right (ListVal []),[])
runComp (eval (List [Const (IntVal 3),Const (IntVal 2), Const NoneVal,Const (StringVal "2")])) [] == (Right (ListVal [IntVal 3,IntVal 2,NoneVal,StringVal "2"]),[])

-- exec
runComp (exec []) [] == (Right (),[])
runComp (exec [SDef "x" (Call "print" [(Var "x")])]) [("x",IntVal 4)] == (Right (),["4"])
runComp (exec [SDef "x" (Call "print" [(Var "x")]), SExp (Oper Plus (Var "x") (Const (IntVal 3))), SDef "x" (Call "print" [(Var "x")])]) [("x",IntVal 4)] == 
  
print(x);x=(x+3);print(x)
-}