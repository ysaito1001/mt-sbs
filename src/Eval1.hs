module Eval1 where

import           Control.Monad.Identity
import qualified Data.Map               as M
import           Types

type Eval1 a = Identity a

runEval1 :: Eval1 a -> a
runEval1 ev = runIdentity ev

eval1 :: Env -> Exp -> Eval1 Value
eval1 env (Lit i) = return $ IntVal i
eval1 env (Var n) =
  maybe (fail ("undefined variable: " ++ n)) return $ M.lookup n env
eval1 env (Plus e1 e2) = do
  e1' <- eval1 env e1
  e2' <- eval1 env e2
  case (e1', e2') of
    (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
eval1 env (Abs n e) = return $ FunVal env n e
eval1 env (App e1 e2) = do
  val1 <- eval1 env e1
  val2 <- eval1 env e2
  case val1 of
    FunVal env' n body -> eval1 (M.insert n val2 env') body
