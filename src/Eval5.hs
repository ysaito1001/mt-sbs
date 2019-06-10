{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Eval5 where

import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import qualified Data.Map               as M
import           Types

newtype Eval5 a =
  Eval5
    (ReaderT Env (ExceptT String (WriterT [String] (StateT Integer Identity))) a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadError String
           , MonadReader Env
           , MonadState Integer
           , MonadWriter [String]
           )

runEval5 :: Env -> Integer -> Eval5 a -> ((Either String a, [String]), Integer)
runEval5 env st (Eval5 m) =
  runIdentity $ runStateT (runWriterT (runExceptT $ runReaderT m env)) st

tick :: (Num s, MonadState s m) => m ()
tick = do
  st <- get
  put (st + 1)

eval5 :: Exp -> Eval5 Value
eval5 (Lit i) = do
  tick
  return $ IntVal i
eval5 (Var n) = do
  tick
  tell [n]
  env <- ask
  case M.lookup n env of
    Nothing  -> throwError ("undefined variable: " ++ n)
    Just val -> return val
eval5 (Plus e1 e2) = do
  tick
  e1' <- eval5 e1
  e2' <- eval5 e2
  case (e1', e2') of
    (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
    _                      -> throwError "type error in addition"
eval5 (Abs n e) = do
  tick
  env <- ask
  return $ FunVal env n e
eval5 (App e1 e2) = do
  tick
  val1 <- eval5 e1
  val2 <- eval5 e2
  case val1 of
    FunVal env' n body -> local (const (M.insert n val2 env')) (eval5 body)
    _                  -> throwError "type error in application"
