module Interpeter where

import AbsReBabel
import PrintReBabel
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Trans.Except
import Data.Map
import ErrM
import Statements
import StateOperations



--transProgram :: Program -> State
--transProgram Porgram stmts =
transFFalse :: FFalse -> Result
transFFalse x = case x of
  FFalse -> failure x
transTTrue :: TTrue -> Result
transTTrue x = case x of
  TTrue -> failure x
transTrueInteger :: TrueInteger -> Result
transTrueInteger x = case x of
  Positive integer -> failure x
  Negative integer -> failure x
transRawCondition :: RawCondition -> Result
transRawCondition x = case x of
  TrueCond ttrue -> failure x
  FFalseCond ffalse -> failure x
  BExpr condition1 boperator condition2 -> failure x
  Equal item1 item2 -> failure x
  Greater item1 item2 -> failure x
  Smaller item1 item2 -> failure x
  Negate condition -> failure x
transCondition :: Condition -> Result
transCondition x = case x of
  Cond rawcondition -> failure x
transOperator :: Operator -> Result
transOperator x = case x of
  DefineOperator type_1 ident type_2 lambda -> failure x
transExpr :: Expr -> Result
transExpr x = case x of
  CallOperator item1 operator item2 -> failure x
  ECall call -> failure x
  MathExpr item1 mathop item2 -> failure x

--transBlock :: Block -> Result
--transBlock x = case x of
--  Block stmts -> failure x
transRBlock :: RBlock -> Result
transRBlock x = case x of
  ReturnBlock stmts rstmt -> failure x
transRStmt :: RStmt -> Result
transRStmt x = case x of
  RBlockStatement rblock -> failure x
  Return item -> failure x
  VoidReturn -> failure x
  RIfStmt condition rblock1 rblock2 -> failure x
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
transLambda :: Lambda -> Result
transLambda x = case x of
  Lambda idents rblock -> failure x

transRefOrVal :: RefOrVal -> Result
transRefOrVal x = case x of
  Ref ident -> failure x
  Val item -> failure x
transCall :: Call -> Result
transCall x = case x of
  Call item reforvals -> failure x
transBOperator :: BOperator -> Result
transBOperator x = case x of
  And -> failure x
  Or -> failure x
  Neither -> failure x
  Xor -> failure x
transMathOp :: MathOp -> Result
transMathOp x = case x of
  Add -> failure x
  Sub -> failure x
  Mul -> failure x
  Div -> failure x





main = do
	args <- getArgs
		-- read from file
		handle <- openFile (head args) ReadMode
		input <- hGetContents handle
		case pProgram (myLexer input) of
			Ok program -> do
				out <- runErrorT (execStateT (runReaderT (runProgram program) emptyEnv) initialSt)
				case out of
					Left err -> hPutStrLn stderr $ "Error: " ++ err
					--Right state -> print $ "State debug: " ++ show state
					Right state -> return ()
			Bad s -> hPutStrLn stderr $ "Parsing failed: " ++ show s
