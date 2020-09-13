-- Skeleton file for Boa Interpreter. Edit only definitions with 'undefined'

module BoaInterp
  (Env, RunError(..), Comp(..),
   abort, look, withBinding, output,
   truthy, operate, apply,
   eval, exec, execute)
  where

import BoaAST
import Control.Monad
import Data.Either (fromRight)

type Env = [(VName, Value)]

data RunError = EBadVar VName | EBadFun FName | EBadArg String
  deriving (Eq, Show)

newtype Comp a = Comp {runComp :: Env -> (Either RunError a, [String]) }

instance Monad Comp where
  return = \a -> Comp (\_ -> (Right a,[mempty])) {-
  (>>=) = \m f -> Comp (\env -> do (a,s') <- runComp m env
                                   (b,s'') <- runComp (f a) env
                                   return [s''])-}
  m >>= f = Comp (\env -> case runComp m env of
                               (Left e, s') -> (Left e, s')
                               (Right a, _) -> runComp (f a) env)

-- You shouldn't need to modify these
instance Functor Comp where
  fmap = liftM
instance Applicative Comp where
  pure = return; (<*>) = ap

-- Operations of the monad
abort :: RunError -> Comp a
abort = \re -> Comp (\_ -> (Left re, mempty))  

look :: VName -> Comp Value
-- ask Mr. Roberto for help big help like please help
look vName = Comp (\env -> (Right ([ v | Just v <- return $ lookup vName env]!!0), mempty))

withBinding :: VName -> Value -> Comp a -> Comp a
withBinding = \vName v m -> Comp (\_ -> runComp m [(vName,v)])

output :: String -> Comp ()
output = \s -> Comp (\_ -> (Right (), [s]))  

-- Helper functions for interpreter
truthy :: Value -> Bool
truthy FalseVal = False
truthy TrueVal = True
truthy _ = error "Not a bool Value"

truthy' :: Bool -> Value
truthy' True = TrueVal
truthy' False = FalseVal

operate :: Op -> Value -> Value -> Either String Value
operate Plus (IntVal v1) (IntVal v2) = Right (IntVal (v1 + v2))  
operate Minus (IntVal v1) (IntVal v2) = Right (IntVal (v1 - v2))  
operate Times (IntVal v1) (IntVal v2) = Right (IntVal (v1 * v2))  
operate Div (IntVal v1) (IntVal v2) = Right (IntVal (v1 `div` v2))   
operate Mod (IntVal v1) (IntVal v2) = Right (IntVal (v1 `mod` v2))  
operate Eq v1 v2 = Right $ truthy' (v1 == v2)
operate Less (IntVal v1) (IntVal v2) = Right $ truthy' (v1 < v2)
operate Greater (IntVal v1) (IntVal v2) = Right $ truthy' (v1 > v2)
operate In v1 (ListVal v2) = Right $ truthy' (v1 `elem` v2) 
operate _ _ _ = Left "Error :("

apply :: FName -> [Value] -> Comp Value
apply "print" [] = return (StringVal "")
apply "print" (x:xs) = return x--(++) <$> (return x) <*> (apply "print" xs)

-- write conditions checks for range
apply "range" [(IntVal n2)] = return $ ListVal (map (\x -> IntVal x) [0..(n2-1)])
apply "range" ((IntVal n1):(IntVal n2):[]) = return $ ListVal (map (\x -> IntVal x) [n1..(n2-1)])
apply "range" ((IntVal n1):(IntVal n2):(IntVal n3):[]) = return $ ListVal (map (\x -> IntVal x) [n1,(n3+1)..(n2-1)])
apply _ _ = abort (EBadArg "WRONG!!!")

-- Main functions of interpreter
eval :: Exp -> Comp Value
eval (Const v) = return v
eval (Var vName) = look vName  
eval (Oper Plus e1 e2) = do res1 <- eval e1
                            res2 <- eval e2
                            return (fromRight (IntVal 0) (operate Plus res1 res2))
eval (Oper Minus e1 e2) = do res1 <- eval e1
                             res2 <- eval e2
                             return (fromRight (IntVal 0) (operate Minus res1 res2))
eval (Oper Times e1 e2) = do res1 <- eval e1
                             res2 <- eval e2
                             return (fromRight (IntVal 0) (operate Times res1 res2))
eval (Oper Div e1 e2) = do res1 <- eval e1
                           res2 <- eval e2
                           return (fromRight (IntVal 0) (operate Div res1 res2)) 
eval (Oper Mod e1 e2) = do res1 <- eval e1
                           res2 <- eval e2
                           return (fromRight (IntVal 0) (operate Mod res1 res2))  
eval (Oper Eq e1 e2) = do res1 <- eval e1
                          res2 <- eval e2
                          return $ truthy' (res1 == res2) 
eval (Oper Less e1 e2) = do res1 <- eval e1
                            res2 <- eval e2
                            return (fromRight FalseVal (operate Less res1 res2)) 
eval (Oper Greater e1 e2) = do res1 <- eval e1
                               res2 <- eval e2
                               return (fromRight FalseVal (operate Greater res1 res2)) 
eval (Oper In e1 e2) = do res1 <- eval e1
                          res2 <- eval e2
                          return res1 -- FIX
eval (Not e) = case e of
                   Const NoneVal -> return TrueVal
                   Const FalseVal -> return TrueVal
                   Const (IntVal 0) -> return TrueVal
                   Const (StringVal "") -> return TrueVal
                   Const (ListVal []) -> return TrueVal
                   otherwise -> return FalseVal
eval (Call fName [e]) = case fName of
                        "print" -> do res <- eval e
                                      apply "print" [res]
                        "range" -> do res <- eval e
                                      apply "range" [res]
eval (List e) =  (fmap eval e)!!0
eval (Compr e [cc]) =   return FalseVal

exec :: Program -> Comp ()
exec = undefined

execute :: Program -> ([String], Maybe RunError)
execute = undefined
