module Lambdas where


import AbsReBabel
import StateOperations

transRBlock :: RBlock -> Result
transRBlock x = case x of
  ReturnBlock stmts rstmt -> failure x

transRStmt :: RStmt -> Result
transRStmt x = case x of
  RBlockStatement rblock -> failure x
  Return item -> failure x
  VoidReturn -> failure x
  RIfStmt condition rblock1 rblock2 -> failure x

transLambda :: Lambda -> Result
transLambda x = case x of
  Lambda idents rblock -> failure x
