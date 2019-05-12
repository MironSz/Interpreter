module Conditions where

import AbsReBabel

transRawCondition :: RawCondition -> Boolean
transRawCondition x = case x of
  TrueCond ttrue -> True
  FFalseCond ffalse -> failure x
  BExpr condition1 boperator condition2 -> failure x
  Equal item1 item2 -> failure x
  Greater item1 item2 -> failure x
  Smaller item1 item2 -> failure x
  Negate condition -> failure x

transCondition :: Condition -> Result
transCondition x = case x of
  Cond rawcondition -> failure x