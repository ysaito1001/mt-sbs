module Eval0 where

import qualified Data.Map   as M
import           Data.Maybe
import           Types

eval0 :: Env -> Exp -> Value
eval0 env (Lit i) = IntVal i
eval0 env (Var n) = fromJust (M.lookup n env)
eval0 env (Plus e1 e2) =
  let IntVal i1 = eval0 env e1
      IntVal i2 = eval0 env e2
   in IntVal (i1 + i2)
eval0 env (Abs n e) = FunVal env n e
eval0 env (App e1 e2) =
  let val1 = eval0 env e1
      val2 = eval0 env e2
   in case val1 of
        FunVal env' n body -> eval0 (M.insert n val2 env') body
