-- This is a skeleton file for you to edit

{-# OPTIONS_GHC -W #-}  -- Just in case you forgot...

module Arithmetic
  (
  showExp,
  evalSimple,
  extendEnv,
  evalFull,
  evalErr,
  showCompact,
  evalEager,
  evalLazy
  )

where

import Definitions
import Data.Maybe -- for fromMaybe
import Data.Either -- for fromLeft, fromRight

showExp :: Exp -> String
showExp (Cst x) = if x<0 then "(" ++ (show x) ++ ")"
                  else show x
showExp (Add x y) = "(" ++ showExp(x) ++ "+" ++ showExp(y) ++ ")"
showExp (Sub x y) = "(" ++ showExp(x) ++ "-" ++ showExp(y) ++ ")"
showExp (Mul x y) = showExp(x) ++ "*" ++ showExp(y)
showExp (Div x y) = showExp(x) ++ "/" ++ showExp(y)
showExp (Pow x y) = showExp(x) ++ "^" ++ showExp(y)
showExp _ = error "Not a \"simple\" operator expression"

evalSimple :: Exp -> Integer
-- https://www.futurelearn.com/courses/functional-programming-haskell/0/steps/27226
evalSimple e =
    case e of
       Cst x -> x
       Add x y -> (evalSimple(x) + evalSimple(y))
       Sub x y -> (evalSimple(x) - evalSimple(y))
       Mul x y -> evalSimple(x) * evalSimple(y)
       Div x y -> evalSimple(x) `div` evalSimple(y)
       Pow x y -> evalSimple(x) ^ evalSimple(y)
       _ -> error "Not a \"simple\" operator expression"

ansEnv :: Env
ansEnv = \v -> if v == "ans" then Just 42 else Nothing

extendEnv :: VName -> Integer -> Env -> Env
extendEnv v n r = \x -> 
                  if v == x then Just n 
                  else r x
-- TO DO Exc 2
evalFull :: Exp -> Env -> Integer
evalFull x env = 
      case x of
        Cst x -> x
        If t y n -> 
               if evalFull t env /= 0 then evalFull y env  
               else evalFull n env
        Var v -> 
               let value = env v in
               if isNothing(value) then error ("Error variable " ++ show (v) ++ " unbounded")
               else fromMaybe 0 value -- https://stackoverflow.com/questions/46363709/haskell-maybe-int-to-int?rq=1
        Let v a b -> evalFull b (extendEnv v (evalFull a env) env)
        Sum v f t b ->
               let env = extendEnv v (evalFull f env) env  in
               if (evalFull f env) > (evalFull t env) then 0
               else (evalFull (Sum v (Cst ((evalFull f env)+1)) t b) (extendEnv v ((evalFull f env)+1) env)) + (evalFull b env)
        Add x y -> evalFull x env + evalFull y env
        Sub x y -> evalFull x env - evalFull y env
        Mul x y -> evalFull x env * evalFull y env
        Div x y -> evalFull x env `div` evalFull y env
        Pow x y -> evalFull x env ^ evalFull y env
        --_ -> error "Upssss something went wrong :P"


-- https://wiki.haskell.org/Handling_errors_in_Haskell
evalErr :: Exp -> Env -> Either ArithError Integer
evalErr e env =
      case e of
        Cst x -> Right x
        If t y n -> 
               if (fromRight 0 (evalErr t env)) /= 0 then evalErr y env  
               else evalErr n env
        Var v -> 
               let value = env v in
               if value == Nothing then Left (EBadVar v)
               else Right (fromMaybe 0 value)
        Let v a b -> 
               let x1 = evalErr a env in
               if isLeft(x1) then x1
               else evalErr b (extendEnv v (fromRight 0 x1) env)
        Sum v f t b ->
               let env = extendEnv v (fromRight 0 (evalErr f env)) env  in
               if (fromRight 0 (evalErr f env)) > (fromRight 0 (evalErr t env)) then Right 0
               else Right ((fromRight 0 (evalErr (Sum v (Cst ((fromRight 0 (evalErr f env))+1)) t b) (extendEnv v ((fromRight 0 (evalErr f env))+1) env))) + (fromRight 0 (evalErr b env)))
        Add x y ->
               let x1 = evalErr x env
                   y1 = evalErr y env in
               if isLeft(x1) then x1
               else if isLeft(y1) then y1
               else Right (fromRight 0 x1 + fromRight 0 y1)
        Sub x y -> 
               let x1 = evalErr x env
                   y1 = evalErr y env in
               if isLeft(x1) then x1
               else if isLeft(y1) then y1
               else Right (fromRight 0 x1 - fromRight 0 y1)
        Mul x y -> 
               let x1 = evalErr x env
                   y1 = evalErr y env in
               if isLeft(x1) then x1
               else if isLeft(y1) then y1
               else Right (fromRight 0 x1 * fromRight 0 y1)
        Div x y -> 
               let x1 = evalErr x env
                   y1 = evalErr y env in
               if isLeft(x1) then x1
               else if isLeft(y1) then y1
               else if fromRight 0 y1 == 0 then Left EDivZero 
               else Right (fromRight 0 x1 `div` fromRight 0 y1)
        Pow x y -> 
               let x1 = evalErr x env
                   y1 = evalErr y env in
               if isLeft(x1) then x1
               else if isLeft(y1) then y1
               else if fromRight 0 y1 < 0 then Left ENegPower
               else Right (fromRight 0 x1 ^ fromRight 0 y1)

-- optional parts (if not attempted, leave them unmodified)

showCompact :: Exp -> String
showCompact = undefined

evalEager :: Exp -> Env -> Either ArithError Integer
evalEager = undefined

evalLazy :: Exp -> Env -> Either ArithError Integer
evalLazy = undefined