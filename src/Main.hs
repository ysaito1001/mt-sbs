module Main where

import           Data.Map
import           Eval6
import           Types

main :: IO ()
main = do
  let e = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))
  runEval6 Data.Map.empty 0 (eval6 e) >>= print
