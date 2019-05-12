module Items where

import AbsReBabel
import StateOperations

transItem :: Item -> Var
transItem x = case x of
  ItemExpr expr -> failure x
  BracesItem item -> failure x
  ItemIdent ident -> failure x
  ItemLiteral trueinteger -> failure x
  ItemString string -> failure x
  ItemLambda lambda -> failure x