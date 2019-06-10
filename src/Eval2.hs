{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Eval2 where

import           Control.Monad.Except
import           Control.Monad.Identity
import qualified Data.Map               as M
import           Types

newtype Eval2 a =
  Eval2 (ExceptT String Identity a)
  deriving (Functor, Applicative, Monad, MonadError String)

runEval2 :: Eval2 a -> Either String a
runEval2 (Eval2 m) = runIdentity (runExceptT m)

eval2 :: Env -> Exp -> Eval2 Value
eval2 env (Lit i) = return $ IntVal i
eval2 env (Var n) =
  case M.lookup n env of
    Nothing  -> throwError ("undefined variable: " ++ n)
    Just val -> return val
eval2 env (Plus e1 e2) = do
  e1' <- eval2 env e1
  e2' <- eval2 env e2
  case (e1', e2') of
    (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
    _                      -> throwError "type error in addition"
eval2 env (Abs n e) = return $ FunVal env n e
eval2 env (App e1 e2) = do
  val1 <- eval2 env e1
  val2 <- eval2 env e2
  case val1 of
    FunVal env' n body -> eval2 (M.insert n val2 env') body
    _                  -> throwError "type error in application"
