{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Eval6 where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import qualified Data.Map             as M
import           Types

newtype Eval6 a =
  Eval6 (ReaderT Env (ExceptT String (WriterT [String] (StateT Integer IO))) a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadError String
           , MonadReader Env
           , MonadState Integer
           , MonadWriter [String]
           , MonadIO
           )

runEval6 ::
     Env -> Integer -> Eval6 a -> IO ((Either String a, [String]), Integer)
runEval6 env st (Eval6 m) =
  runStateT (runWriterT (runExceptT $ runReaderT m env)) st

tick :: (Num s, MonadState s m) => m ()
tick = do
  st <- get
  put (st + 1)

eval6 :: Exp -> Eval6 Value
eval6 (Lit i) = do
  tick
  liftIO $ print i
  return $ IntVal i
eval6 (Var n) = do
  tick
  tell [n]
  env <- ask
  case M.lookup n env of
    Nothing  -> throwError ("undefined variable: " ++ n)
    Just val -> return val
eval6 (Plus e1 e2) = do
  tick
  e1' <- eval6 e1
  e2' <- eval6 e2
  case (e1', e2') of
    (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
    _                      -> throwError "type error in addition"
eval6 (Abs n e) = do
  tick
  env <- ask
  return $ FunVal env n e
eval6 (App e1 e2) = do
  tick
  val1 <- eval6 e1
  val2 <- eval6 e2
  case val1 of
    FunVal env' n body -> local (const (M.insert n val2 env')) (eval6 body)
    _                  -> throwError "type error in application"
