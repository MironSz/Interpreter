module Statements where

import AbsReBabel


transStmtBlock :: Block->State->State
transBlock (Block stmts) s = unnest (foldl (nest s) stmts)


transDecl :: Stmt->State->State
transDecl (Decl type_ ident) state = alloc state type_ ident


transAss ::Stmt->State->State
transAss (Ass ident item) state = assign state ident (type_,)


transAssToLoc ::Maybe Loc->Var->State->State
transAssToLoc Nothing (State store env stateVals) -> State store env (throwError stateVals "Not declared")
transAssToLoc (Just l) v (State store env stateVals) -> State store2 env stateVals where store2 = insert l v store



transStmt :: Stmt -> State->State
transStmt stmt s = case stmt of
  Empty -> s
  BlockStatement block -> transStmtBlock block s
  Decl type_ ident -> transDecl (Decl type_ ident) s
  Ass ident item -> failure x
  IfStmt condition block1 block2 -> failure x
  WhileStmt condition block -> failure x
  PrintStatement item -> failure x
  CallStmt call -> failure x


transBlock :: Block -> Result
transBlock x = case x of
  Block stmts -> failure x