module Types where


import AbsReBabel
import StateOperations

transType :: Type -> Result
transType x = case x of
  VoidT -> failure x
  IntT -> failure x
  BoolT -> failure x
  StringT -> failure x
  FunctionT ftype -> failure x
transFType :: FType -> Result
transFType x = case x of
  Function types type_ -> failure x