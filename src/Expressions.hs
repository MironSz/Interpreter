module Expressions where

import AbsReBabel
import StateOperations



transExpr :: State-> Expr -> (Var,State)
transExpr x = case x of
  CallOperator item1 operator item2 -> failure x
  ECall call -> failure x
  MathExpr item1 mathop item2 -> failure x