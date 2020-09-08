-- Very rudimentary test of Arithmetic. Feel free to replace completely

import Definitions
import Arithmetic

import Data.List (intercalate)
import System.Exit (exitSuccess, exitFailure)  -- for when running stand-alone

tests :: [(String, Bool)]
tests = [test1, test2, test3] where
  test1 = ("test1", evalSimple (Add (Cst 2) (Cst 2)) == 4)
  test2 = ("test2", evalFull (Let "a" (Cst 42) (Var "a")) initEnv == 42)
  test3 = ("test3", evalErr (Var "x") initEnv == Left (EBadVar "x"))

  -- PASSED
  test4 = ("test 4", evalFull (Let "x" (Div (Cst 4) (Cst 0)) (Cst 5)) initEnv == 5)
  test5 = ("test 5", evalFull (Sum "x" (Cst 1) (Cst 0) (Add (Cst 1) (Cst 4))) initEnv == 0)
  test6 = ("test 6", evalFull (Sum "x" (Cst 1) (Cst 5) (Add (Cst 1) (Cst 4))) initEnv == 25)
  test7 = ("test 7", evalFull (Sum "x" (Cst 1) (Cst 5) (Var "x")) initEnv == 15)
  test8 = ("test 8", evalFull (Let {var="x", aux=Cst 5, body= Add (Let {var="x",aux=Add (Cst 3) (Cst 4), body= Mul (Var "x") (Var "x")}) (Var "x")}) initEnv == 54)
  test9 = ("test 9", evalSimple (Add (Cst 2) (Mul (Sub (Cst 20) (Pow (Cst (-5)) (Cst 2))) (Cst (-2)))) == 12)
  test10 = ("test 10", showExp (Add (Cst 2) (Mul (Sub (Cst 20) (Pow (Cst (-5)) (Cst 2))) (Cst (-2))))  == "(2+(20-(-5)^2)*(-2))")
  test11 = ("test 11", evalSimple (Mul (Cst 2) (Add (Cst 3) (Cst 4))) == 14)
  test12 = ("test 12", showExp (Mul (Cst 2) (Add (Cst 3) (Cst 4))) == "2*(3+4)")
  test13 = ("test 13", evalSimple (Add (Mul (Cst 2) (Cst 3)) (Cst 4)) == 10)
  test14 = ("test 14", showExp (Add (Mul (Cst 2) (Cst 3)) (Cst 4)) == "(2*3+4)")
  test15 = ("test 15", evalErr (Var "ans") (extendEnv "ans" 42 initEnv)  == Right 42)
  test16 = ("test 16", evalErr (Div (Cst 1) (Div (Cst 1) (Cst 0))) initEnv == Left EDivZero)
  test17 = ("test 17", evalErr (Div (Cst 1) (Div (Cst 0) (Cst 1))) initEnv == Left EDivZero)
  test18 = ("test 18", evalErr (Div (Cst 0) (Cst 1)) initEnv == Right 0)
  test19 = ("test 19", evalErr (Pow (Cst 3) (Div (Cst 1) (Cst 0))) initEnv == Left EDivZero)
  test20 = ("test 20", evalErr (Pow (Cst 3) (Div (Cst 1) (Cst 1))) initEnv == Right 3)
  test21 = ("test 21", evalErr (Pow (Cst 3) (Cst 1)) initEnv == Right 3)
  test22 = ("test 22", evalErr (Pow (Cst 3) (Cst (-1))) initEnv == Left ENegPower)
  test23 = ("test 23", evalErr (Let "a" (Pow (Cst 1) (Cst (-1))) (Div (Cst 2) (Var "a"))) initEnv == Left ENegPower)
  test24 = ("test 24", evalErr (Let "a" (Pow (Cst 1) (Cst 0)) (Div (Cst 2) (Var "a"))) initEnv == Right 2)
  test25 = ("test 25", evalErr (Let "a" (Cst 0) (Div (Cst 2) (Var "a"))) initEnv == Left EDivZero)
  test26 = ("test 26", evalErr (Let "a" (Cst 0) (Div (Cst 2) (Var "x"))) initEnv == Left (EBadVar "x"))
  test27 = ("test 27", showExp (Pow (Div (Cst 2) (Cst 3)) (Sub (Cst 4) (Cst 5))) == "((2/3)^(4-5))")
  test28 = ("test 28", showExp (Div (Cst 2) (Pow (Cst 3) (Sub (Cst 4) (Cst 5)))) == "(2/(3^(4-5)))")
  test29 = ("test 29", showExp (Mul (Cst 2) (Div (Cst 3) (Cst 4))) == "(2*(3/4))")
  test30 = ("test 30", showExp (Div (Mul (Cst 2) (Cst 3)) (Cst 4)) == "((2*3)/4)")
  test31 = ("test 31", evalErr (If (Var "b1") (Var "b2") (Var "b3")) initEnv == Left (EBadVar "b1"))
  test32 = ("test 32", evalFull (Sum "x" (Cst 1) (Var "x") (Let "x" (Add (Var "x") (Cst 1)) (Var "x"))) (extendEnv "y" 6 (extendEnv "x" 5 initEnv)) == 20)
  test33 = ("test 33", evalFull (Sum "x" (Cst 1) (Var "x") (Sum "x" (Var "x") (Cst 10) (Var "x"))) (extendEnv "y" 6 (extendEnv "x" 5 initEnv)) == 255)
  test34 = ("test 34", evalErr (Sum "x" (Cst 1) (Var "x") (Let "x" (Add (Var "x") (Cst 1)) (Var "x"))) (extendEnv "y" 6 (extendEnv "x" 5 initEnv)) == Right 20)
  test35 = ("test 35", evalErr (Sum "x" (Cst 1) (Var "x") (Sum "x" (Var "x") (Cst 10) (Var "x"))) (extendEnv "y" 6 (extendEnv "x" 5 initEnv)) == Right 255)
  test36 = ("test 36", evalErr (Sum "x" (Var "b1") (Var "b2") (Var "b3")) (extendEnv "y" 6 (extendEnv "x" 5 initEnv)) == Left (EBadVar "b1"))
  
  -- No proper testing just run in console
  --test37 = ("test 37", evalSimple (Pow (Div (Cst 4) (Cst 0)) (Cst 0)) /= 1) --SHOULD BE ERROR expected: Exception: divide by zero
  --test38 = ("test 38", evalSimple (Pow (Pow (Cst 2) (Cst (-1))) (Cst 0)) /= 1) --SHOULD BE ERROR expected: Exception: Negative exponent
  --test39 = ("test 39", evalFull (Pow (Div (Cst 4) (Cst 0)) (Cst 0)) initEnv /= 1) --SHOULD BE ERROR expected: Exception: divide by zero
  --test40 = ("test 40", evalFull (Pow (Pow (Cst 2) (Cst (-1))) (Cst 0)) initEnv /= 1) --SHOULD BE ERROR expected: Exception: Negative exponent
  --test41 = ("test 41", evalFull (Sum "x" (Var "b1") (Var "b2") (Var "b3")) (extendEnv "y" 6 (extendEnv "x" 5 initEnv)) /= 1) -- SHOULD BE ERROR expected: Exception: Error variable "b1" unbounded

main :: IO ()
main =
  let failed = [name | (name, ok) <- tests, not ok]
  in case failed of
       [] -> do putStrLn "All tests passed!"
                exitSuccess
       _ -> do putStrLn $ "Failed tests: " ++ intercalate ", " failed
               exitFailure
