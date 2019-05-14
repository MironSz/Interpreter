module StateOperations where

import Data.Map
import AbsReBabel
import Data.Maybe (fromJust, isJust)

type Loc  =  Int

data Var = IntV Int
  | BoolV Bool
  | StringV String
  | FVar (State->[(Type,Var)]->(Type,Var,State),State,[TypeDecl])
  | Placeholder

data State = State (Map String StateElem) | ErrorState State

type  AEnv a b =  [Map a b]

data StateElem = Store (AEnv Loc (Type,Var))
  | Env (AEnv Ident Loc)
  | Error String
  | MinLoc Loc
  | Output (IO Char)
  | ReturnType [Type]

nestAEnv ::AEnv a b->AEnv a b
nestAEnv aenv = empty:aenv

unnestAEnv :: AEnv a b->AEnv a b
unnestAEnv (x:xs) = xs

getFromAEnv :: Ord a => AEnv a b -> a -> Maybe b
getFromAEnv (x:xs) key = case Data.Map.lookup key x of
	Just val -> Just val
	Nothing -> getFromAEnv xs key
getFromAEnv [] key = Nothing

insertAEnv :: Ord a => AEnv a b -> a -> b -> AEnv a b
insertAEnv (x:xs) key var = (insert key var x):xs

envS = "env"
storeS = "store"
errorS = "error"
minLocS = "minLoc"
returnTypeS = "returnType"

store (State state) =  store2 where Just (Store store2) = Data.Map.lookup storeS state
env (State state) =  env2 where Just (Env env2)= Data.Map.lookup envS state
error (State state) =  error2 where Just (Error error2)= Data.Map.lookup storeS state
minLoc (State state)  =  minLoc2 where Just (MinLoc minLoc2) = Data.Map.lookup storeS state
returnTypeList (State state)  =  type_ where Just (ReturnType type_) = Data.Map.lookup returnTypeS state

addReturnType :: State->Type->State
addReturnType (State state) type_ =
	State (insert returnTypeS (ReturnType (type_:(returnTypeList (State state)))) state)
getReturnType :: State->(State,Type)
getReturnType (State state) = (State (insert returnTypeS (ReturnType restTypes) state) ,type_) where
	type_:restTypes = returnTypeList (State state)

throwError :: State->String->State
throwError a b = a

emptyState :: State
emptyState = State (
  insert storeS (Store [empty])
  (insert envS (Env [empty])
  (insert minLocS (MinLoc  0)  empty)))

get::String->State->StateElem
get s (State state) = fromJust (Data.Map.lookup s state)

checkExistence :: State->Ident->State
checkExistence (State state) ident = case getLoc (State state) ident of
  Just _ -> State state
  Nothing -> throwError (State state) "Undefined "

checkType::State->Ident->Type->State
checkType state _ _  = state

checkReturnType::State->Type->State
checkReturnType  state actualType = if  actualType == expectedType then
	state2 else
	throwError state "Wrong return type" where
		(state2,expectedType) = getReturnType state



nest :: State->State
nest  (State state) =  State (insert storeS nestedStore (insert envS nestedEnv state)) where
	nestedEnv = Env (nestAEnv (env (State state)))
	nestedStore = Store (nestAEnv (store (State state)))

unnest :: State -> State
unnest  (State state) =  State (insert storeS nestedStore (insert envS nestedEnv state)) where
	nestedEnv = Env (unnestAEnv (env (State state)))
	nestedStore = Store (unnestAEnv (store (State state)))

getLoc::State->Ident->Maybe Loc
getLoc state ident = getFromAEnv (env state) ident
getJustLoc::State->Ident->Loc
getJustLoc state ident = fromJust (getLoc state ident)

getVarAndType ::State->Ident->(Type,Var,State)
getVarAndType state ident = case getLoc state ident of
	Nothing -> (VoidT,Placeholder,throwError  state "Undefined ")
	Just loc -> (type_,var,state) where (type_,var) = fromJust (getFromAEnv (store state) loc)

nextLoc::State->(Loc,State)
nextLoc (State state) = (loc,nextState) where
	nextState = State (insert minLocS (MinLoc (minLoc (State state))) state)
	loc = minLoc (State state)

alloc::State->Ident->Type->State
alloc (State state) ident type_ = State (insert envS newEnv (insert storeS newStore state2 )) where
	(loc,State state2) = nextLoc (State state)
	newEnv  = Env (insertAEnv oldEnv ident loc ) where oldEnv = env (State state)
	newStore = Store(insertAEnv oldStore loc (type_ ,Placeholder)) where oldStore = store (State state)
alloc (ErrorState state) _ _ = (ErrorState state)

assign::State->Ident->Type->Var->State
assign (State state) ident type_ var = let (loc,State state2) = nextLoc (State state) in
	let newStore = Store (insertAEnv (store (State state2)) loc (type_,var) ) in
		State (insert storeS newStore state2)


typesFromTypesDecl :: [TypeDecl]->[Type]
typesFromTypesDecl typeDecl = Prelude.map f typeDecl where
	f (TypeDecl type_ ident) = type_

addTypeDeclToEnv :: State->[TypeDecl]->State
addTypeDeclToEnv state typeDecl = Prelude.foldl addTypeDecl state typeDecl where
	addTypeDecl state2 (TypeDecl type_ ident) = alloc state ident type_

addLambdaDeclarations :: State->[(Type,Var)]->State
addLambdaDeclarations s _ = s
