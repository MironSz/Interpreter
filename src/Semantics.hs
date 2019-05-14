module Semantics where

import StateOperations
import AbsReBabel


--evProgram :: Program ->IO Char

evStatementReversed stmt state = evStatement state stmt
evStatement::Stmt->State->State
evStatement _ a = a

evCondition:: Condition->State->(Bool,State)
evCondition (Cond rawCondition) state = evRawCondition rawCondition state

evRawCondition :: RawCondition->State->(Bool,State)
evRawCondition (TrueCond _) state = (True,state)
evRawCondition (FFalseCond _) state = (False,state)
evRawCondition (Negate condition) state = evCondition condition state
--evRawCondition (Equal item1 item2) state =

evItem :: Item->State->(Type,Var,State)
evItem (BracesItem item) state = evItem item state
evItem (ItemLiteral (Positive i)) state = (IntT,IntV 1,state)
evItem (ItemLiteral (Negative i)) state = (IntT,IntV (-1),state)
evItem (ItemString string) state = (StringT,StringV string, state)
evItem (ItemIdent ident) state = getVarAndType state ident
evItem (ItemLambda lambda) state = evLambda lambda state

--evExpression :: Expr->State->(Type,Item,State)
--evExpression ECall (Call item refOrVals) state =

evBlock:: Block->State->State
evBlock (Block stmts) state = unnest (foldl evStatementReversed (nest state) stmts)

evRStmt :: RStmt -> State ->(Type,Var,State)
evRStmt VoidReturn state = (VoidT,Placeholder,checkReturnType state VoidT )
evRStmt (Return item) state=  (type_,var, checkReturnType state2 type_ ) where
	(type_,var,state2) = evItem item state

evRStmt (RIfStmt condition rblock1 rblock2) state = let (isTrue,state2) = evCondition condition state  in
	if isTrue then
		evRStmt (RBlockStatement rblock1) state2 else
		evRStmt (RBlockStatement rblock2) state2

evRBlock  :: RBlock->State->(Type,Var,State)
evRBlock  stmts rstmt

evLambda :: Lambda -> State -> (Type,Var,State)
evLambda (Lambda typeDecls result_type rblock) state = let types = (typesFromTypesDecl typeDecls) in
	(FunctionT types result_type, FVar (func, state, typeDecls), state) where
		func state2 identVars = evRBlock  rblock state2


