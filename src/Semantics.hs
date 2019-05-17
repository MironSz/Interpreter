module Semantics where

import           AbsReBabel
import           StateOperations

evProgram :: Program -> State
evProgram (Program stmts) = foldl evStatementReversed emptyState stmts

evStatementReversed stmt state = evStatement state stmt

evStatement :: Stmt -> State -> State
evStatement Empty s = s
evStatement (Decl type_ ident) state = alloc state ident type_
evStatement (Ass ident item) state = assign state2 ident type_ var
  where
    (type_, var, state2) = evItem item state
evStatement (IfStmt condition block1 block2) state =
  let (isTrue, state2) = evCondition condition state
   in if isTrue
        then evBlock block1 state2
        else evBlock block2 state2
evStatement (BlockStatement block) state = evBlock block state
evStatement (CallStmt call) state = state2
  where
    (_, _, state2) = evCallLambda call state

evCondition :: Condition -> State -> (Bool, State)
evCondition (Cond rawCondition) = evRawCondition rawCondition

evRawCondition :: RawCondition -> State -> (Bool, State)
evRawCondition (TrueCond _) state          = (True, state)
evRawCondition (FFalseCond _) state        = (False, state)
evRawCondition (Negate condition) state    = evCondition condition state
evRawCondition (Equal item1 item2) state   = evCompare item1 item2 (==) state
evRawCondition (Smaller item1 item2) state = evCompare item1 item2 (<) state
evRawCondition (Greater item1 item2) state = evCompare item1 item2 (>) state

evCompare :: Item -> Item -> (Var -> Var -> Bool) -> State -> (Bool, State)
evCompare item1 item2 f state =
  let (type1, var1, state1) = evItem item1 state
   in let (type2, var2, state2) = evItem item2 state1
       in (f var1 var2, state2)

--evRawCondition (Equal item1 item2) state =
evItem :: Item -> State -> (Type, Var, State)
evItem (BracesItem item) state          = evItem item state
evItem (ItemLiteral (Positive i)) state = (IntT, IntV i, state)
evItem (ItemLiteral (Negative i)) state = (IntT, IntV (-i), state)
evItem (ItemString string) state        = (StringT, StringV string, state)
evItem (ItemIdent ident) state          = getVarAndType state ident
evItem (ItemLambda lambda) state        = evDefLambda lambda state
evItem (ItemExpr expr) state            = evExpression expr state

evExpression :: Expr -> State -> (Type, Var, State)
evExpression (MathExpr item1 op item2) state =
  let (type1, var1, state1) = evItem item1 state
   in let (type2, var2, state2) = evItem item2 state1
       in case (type1, type2) of
            (IntT, IntT) -> evMathOp op state2 var1 var2
evExpression (ECall call) state = evCallLambda call state

--evExpression ECall (Call item refOrVals) state =
evMathOp :: MathOp -> State -> (Var -> Var -> (Type, Var, State))
evMathOp op state = f
  where
    f (IntV a) (IntV b) = (IntT, var, state)
      where
        var =
          case op of
            Add -> IntV (a + b)
            Sub -> IntV (a - b)
            Mul -> IntV (a * b)

evBlock :: Block -> State -> State
evBlock (Block stmts) state = unnest (foldl evStatementReversed (nest state) stmts)

evRStmt :: RStmt -> State -> (Type, Var, State)
evRBlock :: RBlock -> State -> (Type, Var, State)
evRBlock (ReturnBlock stmts rstmt) state = evRStmt rstmt state2
  where
    state2 = foldl evStatementReversed state stmts

evRStmt (Return item) state = (type_, var, state2)
  where
    (type_, var, state2) = evItem item state
evRStmt (RIfStmt condition rblock1 rblock2) state =
  let (isTrue, state2) = evCondition condition state
   in if isTrue
        then evRStmt (RBlockStatement rblock1) state2
        else evRStmt (RBlockStatement rblock2) state2
evRStmt VoidReturn state = (VoidT, Placeholder, state)

evDefLambda :: Lambda -> State -> (Type, Var, State)
evDefLambda (Lambda typeDecls result_type rblock) state =
  let types = typesFromTypesDecl typeDecls
   in (FunctionT types result_type, FVar (func, addTypeDeclToEnv state typeDecls, typeDecls), state)
  where
    func = evRBlock rblock

--  let stateWithDeclared =
evCallLambda :: Call -> State -> (Type, Var, State)
evCallLambda (Call item refOrVals) state =
  let (lambdaType, lambda, state2) = evItem item state
   in let lambdaState = loadLambdaVars state2 lambda refOrVals
       in let lambdaState2 = addSelf lambdaState lambda
           in let (resultType, resultVar, newLambdaState) = performLambda lambdaState2 lambda
               in let (state2, newLambdaState2) = retrieveRef state newLambdaState (zip refOrVals (typeDecl lambda))
                   in case item of
                        ItemIdent ident -> (resultType, resultVar, updateLambdaState state2 newLambdaState2 ident)
                        _ -> (resultType, resultVar, state2)

updateLambdaState :: State -> State -> Ident -> State
updateLambdaState globalState lambdaState ident =
  let (type_, FVar (func, oldLambdaState, types), globalState2) = getVarAndType globalState ident
   in assign globalState2 ident type_ (FVar (func, lambdaState, types))

typeDecl :: Var -> [TypeDecl]
typeDecl (FVar (_, _, xs)) = xs

retrieveRef :: State -> State -> [(RefOrVal, TypeDecl)] -> (State, State)
retrieveRef s1 s2 = foldl retrieveSingleRef (s1, s2)

retrieveSingleRef :: (State, State) -> (RefOrVal, TypeDecl) -> (State, State)
retrieveSingleRef (globalState, lambdaState) (Ref identInGlobal, TypeDecl _ identInLambda) =
  let (type_, var, _) = getVarAndType lambdaState identInLambda
   in (assign globalState identInGlobal type_ var, lambdaState)
retrieveSingleRef (s1, s2) _ = (s1, s2)

performLambda :: State -> Var -> (Type, Var, State)
performLambda lambdaState (FVar (func, _, _)) = func lambdaState

addSelf :: State -> Var -> State
addSelf s _ = s

addTypeDeclToEnv :: State -> [TypeDecl] -> State
addTypeDeclToEnv state typeDecl = Prelude.foldl addTypeDecl state typeDecl
  where
    addTypeDecl state2 (TypeDecl type_ ident) = alloc state ident type_

--Zwraca state z jakim ma być wywołana lambda
loadLambdaVars :: State -> Var -> [RefOrVal] -> State
loadLambdaVars globalState (FVar (func, lambdaState, typeDecl)) refOrVals =
  let identAndRefOrVal = zip typeDecl refOrVals
   in foldl (addToLambda globalState) lambdaState identAndRefOrVal

addToLambda :: State -> State -> (TypeDecl, RefOrVal) -> State
addToLambda globalState state (td, Ref ident) = addToLambda globalState state (td, Val (ItemIdent ident))
addToLambda globalState state (TypeDecl type_ ident, Val item) = assign state ident type_ val
  where
    (type_, val, state2) = evItem item globalState
