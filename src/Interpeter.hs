module Interpeter where

import AbsReBabel
import PrintReBabel
import Data.Map
import ErrM
import StateOperations


--------------Program---------
--transProgram :: Program -> State
--transProgram Porgram stmts =

------------------Statements---------


transStmtBlock :: Block->State->State
transStmtBlock (Block stmts) state = unnest (Prelude.foldl transStmt (nest state)  stmts)


transDecl :: Stmt->State->State
transDecl (Decl type_ ident) state = alloc state type_ ident


transAss ::Stmt->State->State
transAss (Ass ident item) state = assign state2 ident (type_,var) where
  (type_,var,state2) = transItem state item


--transAssToLoc ::Maybe Loc->Var->State->State
--transAssToLoc Nothing (State store env stateVals) -> State store env (throwError stateVals "Not declared")
--transAssToLoc (Just l) v (State store env stateVals) -> State store2 env stateVals where store2 = insert l v store
--


transStmt :: State->Stmt ->State
transStmt state stmt  = case stmt of
  Empty -> state
  BlockStatement block -> transStmtBlock block state
  Decl type_ ident -> transDecl (Decl type_ ident) state
--  Ass ident item -> failure x
--  IfStmt condition block1 block2 -> failure state
--  WhileStmt condition block -> failure state
--  PrintStatement item -> failure x
--  CallStmt call -> failure x




-----------Items--------------

transItem :: State->Item -> (Type,Var,State)
transItem  state x = case x of
  ItemExpr expr -> (type_,val,state) where (type_,val,state)= transExpr state expr
  BracesItem item -> transItem state item
  ItemIdent ident -> getJustTypeVar state ident
  ItemLiteral trueinteger -> (IntT, IntV 1, state)
  ItemString string -> (StringT,StringV string,state)
  ItemLambda lambda -> (type_, var,state) where (type_,var) = transLambda lambda

--Muszę stworzyć prawdziwy Ftype



-----------Types----------------


--transType :: Type -> Result
--transType x = case x of
--  VoidT -> failure x
--  IntT -> failure x
--  BoolT -> failure x
--  StringT -> failure x
--  FunctionT ftype -> failure x
--transFType :: FType -> Result
--transFType x = case x of
--  Function types type_ -> failure x



------------------Lambdas--------------------

transTypeDecl :: State->TypeDecl -> State
transTypeDecl state x = case x of
  TypeDecl type_ ident -> alloc state type_ ident

transRBlock :: State-> RBlock -> (Type,Var,State)
transRBlock state x = case x of
  ReturnBlock stmts rstmt -> transRStmt state2 rstmt where state2 = transStmtBlock  stmts state


transRStmt :: State->RStmt -> (Type,Var,State)
transRStmt state x = case x of
  RBlockStatement rblock -> transRBlock rblock
  Return item -> transItem state item
  VoidReturn -> (Placeholder,state)
  RIfStmt condition rblock1 rblock2 -> transRBlock rblock where
    rblock =  if transCondition condition then rblock1 else rblock2

transLambda :: Lambda -> (Type,Var)
transLambda x = case x of
  Lambda typedecls type_ rblock -> (fType, FVar func) where
    func state vars = (type_, var, unnest resultState) where
      (var,resultState) = transRBlock lambdaState rblock where
        lambdaState = Prelude.foldl transTypeDecl nestedState typedecls where
          nestedState = nest state where
            fType = FunctionT (Function types type_) where
              types = Prelude.map detupple typedecls where
                detupple (TypeDecl type_ ident) = type_



------------------Expressions----------------------


transExpr :: State-> Expr -> (Type,Var,State)
transExpr x = case x of
  CallOperator item1 operator item2 -> failure x
  ECall call -> failure x
  MathExpr item1 mathop item2 -> failure x





--------------------Conditions------------------------------


transRawCondition :: RawCondition -> Bool
transRawCondition x = case x of
  TrueCond ttrue -> True
  FFalseCond ffalse -> failure x
  BExpr condition1 boperator condition2 -> failure x
  Equal item1 item2 -> failure x
  Greater item1 item2 -> failure x
  Smaller item1 item2 -> failure x
  Negate condition -> not transRawCondition condition


type Result = String

--
--transCondition :: Condition -> Result
--transCondition x = case x of
--  Cond rawcondition -> transRawCondition rawcondition
--
--
--transFFalse :: FFalse -> Result
--transFFalse x = case x of
--  FFalse -> failure x
--transTTrue :: TTrue -> Result
--transTTrue x = case x of
--  TTrue -> failure x
--transTrueInteger :: TrueInteger -> Result
--transTrueInteger x = case x of
--  Positive integer -> failure x
--  Negative integer -> failure x
--transRawCondition :: RawCondition -> Result
--transRawCondition x = case x of
--  TrueCond ttrue -> failure x
--  FFalseCond ffalse -> failure x
--  BExpr condition1 boperator condition2 -> failure x
--  Equal item1 item2 -> failure x
--  Greater item1 item2 -> failure x
--  Smaller item1 item2 -> failure x
--  Negate condition -> failure x
--transCondition :: Condition -> Result
--transCondition x = case x of
--  Cond rawcondition -> failure x
--transOperator :: Operator -> Result
--transOperator x = case x of
--  DefineOperator type_1 ident type_2 lambda -> failure x
--transExpr :: Expr -> Result
--transExpr x = case x of
--  CallOperator item1 operator item2 -> failure x
--  ECall call -> failure x
--  MathExpr item1 mathop item2 -> failure x
--
----transBlock :: Block -> Result
----transBlock x = case x of
----  Block stmts -> failure x
--transRBlock :: RBlock -> Result
--transRBlock x = case x of
--  ReturnBlock stmts rstmt -> failure x
--transRStmt :: RStmt -> Result
--transRStmt x = case x of
--  RBlockStatement rblock -> failure x
--  Return item -> failure x
--  VoidReturn -> failure x
--  RIfStmt condition rblock1 rblock2 -> failure x
--transType :: Type -> Result
--transType x = case x of
--  VoidT -> failure x
--  IntT -> failure x
--  BoolT -> failure x
--  StringT -> failure x
--  FunctionT ftype -> failure x
--transFType :: FType -> Result
--transFType x = case x of
--  Function types type_ -> failure x
--transLambda :: Lambda -> Result
--transLambda x = case x of
--  Lambda idents rblock -> failure x
--
--transRefOrVal :: RefOrVal -> Result
--transRefOrVal x = case x of
--  Ref ident -> failure x
--  Val item -> failure x
--transCall :: Call -> Result
--transCall x = case x of
--  Call item reforvals -> failure x
--transBOperator :: BOperator -> Result
--transBOperator x = case x of
--  And -> failure x
--  Or -> failure x
--  Neither -> failure x
--  Xor -> failure x
--transMathOp :: MathOp -> Result
--transMathOp x = case x of
--  Add -> failure x
--  Sub -> failure x
--  Mul -> failure x
--  Div -> failure x
--


--
--
--main = do
--	args <- getArgs
--		-- read from file
--		handle <- openFile (head args) ReadMode
--		input <- hGetContents handle
--		case pProgram (myLexer input) of
--			Ok program -> do
--				out <- runErrorT (execStateT (runReaderT (runProgram program) emptyEnv) initialSt)
--				case out of
--					Left err -> hPutStrLn stderr $ "Error: " ++ err
--					--Right state -> print $ "State debug: " ++ show state
--					Right state -> return ()
--			Bad s -> hPutStrLn stderr $ "Parsing failed: " ++ show s
