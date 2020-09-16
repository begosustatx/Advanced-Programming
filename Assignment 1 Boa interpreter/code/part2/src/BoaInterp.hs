-- Skeleton file for Boa Interpreter. Edit only definitions with 'undefined'

module BoaInterp
  (Env, RunError(..), Comp(..),
   abort, look, withBinding, output,
   truthy, operate, apply,
   eval, exec, execute, truthy', convertToString)
  where

import BoaAST
import Control.Monad
import Data.List (intercalate)

type Env = [(VName, Value)]

data RunError = EBadVar VName | EBadFun FName | EBadArg String
  deriving (Eq, Show)

newtype Comp a = Comp {runComp :: Env -> (Either RunError a, [String]) }

instance Monad Comp where
  return = \a -> Comp (\_ -> (Right a,[]))
  m >>= f = Comp (\env -> case runComp m env of
                               (Left e, s') -> (Left e, s')
                               (Right a, s') -> case runComp (f a) env of
                                                  (Left e', s'') -> (Left e',s'<>s'')
                                                  (Right b, s'') -> (Right b, s'<>s''))

-- You shouldn't need to modify these
instance Functor Comp where
  fmap = liftM
instance Applicative Comp where
  pure = return; (<*>) = ap

-- Operations of the monad
abort :: RunError -> Comp a
abort = \re -> Comp (\_ -> (Left re, mempty))  

look :: VName -> Comp Value
look vName = Comp (\env -> case lookup vName env of 
                            Nothing -> (Left (EBadVar vName), mempty)
                            Just v  -> (Right v, mempty))

withBinding :: VName -> Value -> Comp a -> Comp a
withBinding = \vName v m -> Comp (\env -> runComp m ([(vName,v)]<>env))

output :: String -> Comp ()
output = \s -> Comp (\_ -> (Right (), [s]))  

-- Helper functions for interpreter
truthy :: Value -> Bool
truthy NoneVal = False
truthy FalseVal = False
truthy (IntVal 0) = False
truthy (StringVal "") = False
truthy (ListVal []) = False
truthy _ = True

truthy' :: Bool -> Value
truthy' True = TrueVal
truthy' False = FalseVal

operate :: Op -> Value -> Value -> Either String Value
operate Plus (IntVal v1) (IntVal v2) = Right (IntVal (v1 + v2))  
operate Minus (IntVal v1) (IntVal v2) = Right (IntVal (v1 - v2))  
operate Times (IntVal v1) (IntVal v2) = Right (IntVal (v1 * v2))  
operate Div (IntVal v1) (IntVal v2) = if v2 == 0 then Left "Divide by Zero"
                                      else Right (IntVal (v1 `div` v2))   
operate Mod (IntVal v1) (IntVal v2) = if v2 == 0 then Left "Modulo by Zero"
                                      else Right (IntVal (v1 `mod` v2))  
operate Eq v1 v2 = Right $ truthy' (v1 == v2)
operate Less (IntVal v1) (IntVal v2) = Right $ truthy' (v1 < v2)
operate Greater (IntVal v1) (IntVal v2) = Right $ truthy' (v1 > v2)
operate In v1 (ListVal v2) = Right $ truthy' (v1 `elem` v2) 
operate _ _ _ = Left "The values must be proper for chosen operator"

convertToString :: Value -> String
convertToString NoneVal = "None"
convertToString TrueVal = "True"
convertToString FalseVal = "False"
convertToString (IntVal x) = show x
convertToString (StringVal x) = x
convertToString (ListVal []) = "[]"
convertToString (ListVal xs) = "[" ++ intercalate ", " (map convertToString xs) ++ "]"

apply :: FName -> [Value] -> Comp Value
apply "print" x = do ; output(unwords(map convertToString x)); return NoneVal
apply "range" [IntVal n2] = return $ ListVal (map IntVal [0..(n2-1)])
apply "range" [IntVal n1,IntVal n2] = return $ ListVal (map IntVal [n1..(n2-1)])
apply "range" [IntVal n1,IntVal n2,IntVal n3]  
                                             | n1<=n2 && n3<0 = return $ ListVal []
                                             | n1>n2 && n3<0 = return $ ListVal (map IntVal [n1,(n3+n1)..(n2+1)])
                                             | otherwise = return $ ListVal (map IntVal [n1,(n3+n1)..(n2-1)])
apply "range" x = abort (EBadArg (show x))                                                      
apply fName _ = abort (EBadFun fName)

-- Main functions of interpreter
eval :: Exp -> Comp Value
eval (Const v) = return v
eval (Var vName) = look vName  
eval (Oper Plus e1 e2) = do res1 <- eval e1
                            res2 <- eval e2
                            case operate Plus res1 res2 of
                                 Left s -> abort (EBadFun s)
                                 Right v -> return v
eval (Oper Minus e1 e2) = do res1 <- eval e1
                             res2 <- eval e2
                             case operate Minus res1 res2 of
                                 Left s -> abort (EBadFun s)
                                 Right v -> return v
eval (Oper Times e1 e2) = do res1 <- eval e1
                             res2 <- eval e2
                             case operate Times res1 res2 of
                                 Left s -> abort (EBadFun s)
                                 Right v -> return v
eval (Oper Div e1 e2) = do res1 <- eval e1
                           res2 <- eval e2
                           case operate Div res1 res2 of
                                 Left s -> abort (EBadFun s)
                                 Right v -> return v
eval (Oper Mod e1 e2) = do res1 <- eval e1
                           res2 <- eval e2
                           case operate Mod res1 res2 of
                                 Left s -> abort (EBadFun s)
                                 Right v -> return v
eval (Oper Eq e1 e2) = do res1 <- eval e1
                          res2 <- eval e2
                          return $ truthy' (res1 == res2) 
eval (Oper Less e1 e2) = do res1 <- eval e1
                            res2 <- eval e2
                            case operate Less res1 res2 of
                                 Left s -> abort (EBadFun s)
                                 Right v -> return v
eval (Oper Greater e1 e2) = do res1 <- eval e1
                               res2 <- eval e2
                               case operate Greater res1 res2 of
                                    Left s -> abort (EBadFun s)
                                    Right v -> return v
eval (Oper In e1 e2) = do res1 <- eval e1
                          res2 <- eval e2
                          case operate In res1 res2 of
                               Left s -> abort (EBadFun s)
                               Right v -> return v 
eval (Not e) = do r <- eval e
                  return (truthy' (not (truthy r)))
eval (Call fName e) = do res <- eval (List e)
                         case res of
                           (ListVal x) -> apply fName x
                           _ -> abort (EBadArg "The argument must be a List")
eval (List []) = return (ListVal [])
eval (List (x:xs)) = do res1 <- eval x
                        res2 <- eval (List xs)
                        case res2 of
                          (ListVal x) -> return (ListVal (res1:x))
                          _ -> abort (EBadArg "The argument must be a List")
-- Needs to be fixed 
eval (Compr e []) = eval e
eval (Compr e ((CCFor var exp):xs)) = do result <- eval exp
                                         case result of
                                            (ListVal vs) -> do bind <- mapM (\v -> withBinding var v (eval (Compr e xs))) vs
                                                               return (ListVal bind)
                                            _   ->  abort (EBadArg "The argument must be a List")
eval (Compr e ((CCIf exp):xs)) = do result <- eval exp
                                    if truthy result then eval e
                                    else eval (Compr e xs) 

exec :: Program -> Comp ()
exec [] = return ()
exec (x:xs) = case x of 
              (SDef vName exp) -> do 
                                    res1 <- eval exp
                                    withBinding vName res1 (exec xs)
              (SExp exp) -> do 
                              eval exp
                              exec xs

execute :: Program -> ([String], Maybe RunError)
execute p = let prog = runComp (exec p) [] in
              case prog of
              (Right _, s) -> (s, Nothing)
              (Left x, s)  -> (s, Just x)