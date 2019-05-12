module Items where

import AbsReBabel
import StateOperations


transItem :: State->Item -> (Var,State)
transItem x state = case x of
  ItemExpr expr -> transExpr expr
  BracesItem item -> transItem item
  ItemIdent ident -> getVar
  ItemLiteral trueinteger -> failure x
  ItemString string -> String string
  ItemLambda lambda -> failure x