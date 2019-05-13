module StateOperations where

import Data.Map
import AbsReBabel
import Data.Maybe (fromJust, isJust)

type Loc  =  Int

data Var = IntV Int
  | BoolV Bool
  | StringV String
  | FVar (State->[Var]->(Type,Var,State))
  | Placeholder

type State = Map String StateElem

type AEnv a = Map Ident a

data StateElem = Store (Map Loc Var)
  | Env [AEnv Loc]
  | TEnv [AEnv Type]
  | Error String
  | MinLoc Loc


envS::String
envS = "env"

tenvS::String
tenvS = "tenv"

storeS::String
storeS = "store"

errorS::String
errorS = "error"

minLocS::String
minLocS = "minLoc"


emptyState :: State
emptyState =
  insert storeS (Store empty)
  (insert tenvS (TEnv  [empty])
  (insert envS (Env [empty])
  (insert minLocS (MinLoc  0)  empty)))

get::String->State->StateElem
get s state = fromJust (Data.Map.lookup s state)

getEnv :: State->[AEnv Loc]
getEnv s = case get envS s of
  Env xs -> xs
getTEnv :: State->[AEnv Type]
getTEnv s = case get tenvS s of
  TEnv xs -> xs

checkExistence :: State->Ident->State
checkExistence state ident = case getLoc (get envS state) ident of
  Just _ -> state
  Nothing -> throwError state "Undefined "

nest :: State->State
nest  state =  insert tenvS nestedTEnv (insert envS nestedEnv state) where
  nestedEnv = Env (empty:(getEnv state))
  nestedTEnv = TEnv (empty:(getTEnv state))

unnestEnv :: StateElem->StateElem
unnestEnv (Env (x:xs)) = (Env xs)
unnestEnv (TEnv (x:xs)) = (TEnv xs)


unnest :: State -> State
unnest state = insert envS oldEnv (insert tenvS oldTEnv state) where
  oldEnv = unnestEnv (get envS state)
  oldTEnv = unnestEnv (get tenvS state)

getLoc :: StateElem->Ident->Maybe Loc
getLoc (Env (e:es)) id = case Data.Map.lookup id e of
  Just l -> Just l
  Nothing -> getLoc (Env es) id
getLoc (Env []) id = Nothing

getType :: StateElem ->Ident ->Maybe Type
getType (TEnv (e:es)) id = case Data.Map.lookup id e of
  Just l -> Just l
  Nothing -> getType (TEnv es) id
getType (TEnv []) id = Nothing

getTType ::State->Ident->Type
getTType state ident = fromJust (getType (get tenvS state) ident)

throwError ::State->String->State
throwError state s =  state

getVarFromLoc :: StateElem->Maybe Loc ->Maybe Var
getVarFromLoc (Store s) (Just l) = Data.Map.lookup l s
getVarFromLoc _ _ = Nothing

getVar :: State ->Ident-> Maybe Var
getVar store id = getVarFromLoc (get storeS store) (getLoc (get envS store) id)

getJustTypeVar :: State->Ident -> (Type,Var,State)
getJustTypeVar state ident = (type_, var, state2) where
  state2 = checkExistence state ident
  (type_,var) = case getVar state ident of
    Just var -> (getTType state ident, var)
    Nothing  -> (Whatever,Placeholder)

getNextLoc ::State->(State,Loc)
getNextLoc state = (insert minLocS (MinLoc (nextLoc+1)) state,nextLoc) where
  MinLoc nextLoc = get minLocS state

addToEnv :: State->Ident->State
addToEnv state id = insert envS (Env (insert id loc env:envs)) state2 where
  Env (env:envs) = get envS state
  ( state2 , loc) = getNextLoc state

addToTEnv :: State->Ident->Type->State
addToTEnv state id type_ = insert tenvS newTEnv state where
  newTEnv = TEnv (insert id type_ oldTenv:tenvs)
    where
      TEnv (oldTenv:tenvs) = get tenvS state

alloc :: State->Type->Ident->State
alloc state type_ id = addToTEnv (addToEnv state id ) id type_

assign :: State->Ident->(Type,Var)->State
assign state id (type_, var) =
  if type_2 /= Just type_ && isJust type_2
    then
      throwError state "Wrong type"
    else
      case loc of
        Just l -> insert storeS (Store (insert l var store)) state
        Nothing -> throwError state "Nor defined"
  where
    Store store = get storeS state
    loc = getLoc (get envS state) id
    type_2 = getType (get tenvS state) id

