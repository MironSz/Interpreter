module StateOperations where

import AbsReBabel
import Data.Map
import Data.Maybe (fromJust, isJust)

type Loc = Int

data Var
  = IntV Integer
  | BoolV Bool
  | StringV String
  | FVar (State -> (Type, Var, State), State, [TypeDecl])
  | Placeholder

instance Show Var where
  show (FVar _) = "some function"
  show (IntV i) = show i
  show Placeholder = "placeholder"
  show _ = "sth"

instance Eq Var where
  x == y =
    case (x, y) of
      (IntV i, IntV j) -> i == j
      (BoolV i, BoolV j) -> i == j
      (StringV i, StringV j) -> i == j
      (Placeholder, Placeholder) -> True
      (_, _) -> False

instance Ord Var where
  x `compare` y =
    case (x, y) of
      (IntV i, IntV j) -> i `compare` j
      (BoolV i, BoolV j) -> i `compare` j
      (StringV i, StringV j) -> i `compare` j

data State
  = State (Map String StateElem)
  | ErrorState State String

instance Show State where
  show (ErrorState state error) =
    "Error occured: " ++ error ++ "\n\nState before error:\n" ++ show state
  show state =
    "store: " ++
    show (Store (store state)) ++
    "env: " ++
    show (Env (env state)) ++
    "minLoc: " ++ show (minLoc state) ++ "\n" ++ "msg:  " ++ show (msg state)

type AEnv a b = [Map a b]

showAEnv ::
     Show key
  => Show val =>
       AEnv key val -> String
showAEnv (x:xs) = "\n" ++ show x ++ "\n------------\n" ++ showAEnv xs
showAEnv [] = ""

data StateElem
  = Store (AEnv Loc (Type, Var))
  | Env (AEnv Ident Loc)
  | MinLoc Loc
  | ReturnType [Type]
  | Msg [String]

instance Show StateElem where
  show (Store env) = showAEnv env
  show (Env env) = showAEnv env
  show (Msg str) = show str
  show (MinLoc loc) = show loc
  show (ReturnType types) = show types

--  | Output (IO Char)
nestAEnv :: AEnv a b -> AEnv a b
nestAEnv aenv = empty : aenv

unnestAEnv :: AEnv a b -> AEnv a b
unnestAEnv (x:xs) = xs

getFromAEnv :: Ord a => AEnv a b -> a -> Maybe b
getFromAEnv (x:xs) key =
  case Data.Map.lookup key x of
    Just val -> Just val
    Nothing -> getFromAEnv xs key
getFromAEnv [] key = Nothing

insertAEnv :: Ord a => AEnv a b -> a -> b -> AEnv a b
insertAEnv (x:xs) key var = (insert key var x) : xs

envS = "env"

storeS = "store"

errorS = "error"

minLocS = "minLoc"

returnTypeS = "returnType"

msgS = "messege"

store (State state) = store2
  where
    Just (Store store2) = Data.Map.lookup storeS state

env (State state) = env2
  where
    Just (Env env2) = Data.Map.lookup envS state

msg (State state) = msg2
  where
    Just (Msg msg2) = Data.Map.lookup msgS state

--error (State state) = error2
--  where
--    Just (Error error2) = Data.Map.lookup errorS state
minLoc (State state) = minLoc2
  where
    Just (MinLoc minLoc2) = Data.Map.lookup minLocS state

returnTypeList (State state) = type_
  where
    Just (ReturnType type_) = Data.Map.lookup returnTypeS state

addReturnType :: State -> Type -> State
addReturnType s _ = s
addReturnType (State state) type_ =
  State (insert returnTypeS (ReturnType (type_ : (returnTypeList (State state)))) state)

getReturnType :: State -> (State, Type)
getReturnType (State state) = (State (insert returnTypeS (ReturnType restTypes) state), type_)
  where
    type_:restTypes = returnTypeList (State state)

throwError :: State -> String -> State
throwError (ErrorState state error) _ = ErrorState state error
throwError state error = (ErrorState state error)

emptyState :: State
emptyState =
  State
    (insert
       storeS
       (Store [empty])
       (insert
          envS
          (Env [empty])
          (insert
             minLocS
             (MinLoc 0)
             (insert returnTypeS (ReturnType []) (insert msgS (Msg []) empty)))))

get :: String -> State -> StateElem
get s (State state) = fromJust (Data.Map.lookup s state)

checkExistence :: State -> Ident -> State
checkExistence (ErrorState state error) _ = (ErrorState state error)
checkExistence (State state) ident =
  case getLoc (State state) ident of
    Just _ -> State state
    Nothing -> throwError (State state) "Undefined "

checkType :: State -> Ident -> Type -> State
checkType (ErrorState state error) _ _ = (ErrorState state error)
checkType state _ _ = state

checkReturnType :: State -> Type -> State
checkReturnType (ErrorState state error) _ = (ErrorState state error)
checkReturnType state actualType =
  if actualType == expectedType
    then state2
    else throwError state "Wrong return type"
  where
    (state2, expectedType) = getReturnType state

nest :: State -> State
nest (ErrorState state error) = (ErrorState state error)
nest (State state) = State (insert storeS nestedStore (insert envS nestedEnv state))
  where
    nestedEnv = Env (nestAEnv (env (State state)))
    nestedStore = Store (nestAEnv (store (State state)))
nest e = e

unnest :: State -> State
unnest (ErrorState state error) = (ErrorState state error)
unnest (State state) = State (insert storeS nestedStore (insert envS nestedEnv state))
  where
    nestedEnv = Env (unnestAEnv (env (State state)))
    nestedStore = Store (unnestAEnv (store (State state)))

getLoc :: State -> Ident -> Maybe Loc
getLoc state ident = getFromAEnv (env state) ident

getJustLoc :: State -> Ident -> Loc
getJustLoc (ErrorState state error) _ = 0
getJustLoc state ident = fromJust (getLoc state ident)

getVarAndType :: State -> Ident -> (Type, Var, State)
getVarAndType (ErrorState state error) _ = (VoidT, Placeholder, (ErrorState state error))
getVarAndType state ident =
  case getLoc state ident of
    Nothing -> (VoidT, Placeholder, throwError state "Undefined ")
    Just loc -> (type_, var, state)
      where (type_, var) = fromJust (getFromAEnv (store state) loc)

nextLoc :: State -> (Loc, State)
nextLoc (ErrorState state error) = (0, (ErrorState state error))
nextLoc (State state) = (loc, nextState)
  where
    nextState = State (insert minLocS (MinLoc (minLoc (State state) + 1)) state)
    loc = minLoc (State state)

alloc :: State -> Ident -> Type -> State
alloc (ErrorState state error) _ _ = (ErrorState state error)
alloc state ident type_ =
  let (loc, State locState) = nextLoc state
   in let oldEnv = env (State locState)
       in let newEnv = Env (insertAEnv oldEnv ident loc)
           in let oldStore = store (State locState)
               in let newStore = Store (insertAEnv oldStore loc (type_, Placeholder))
                   in State (insert storeS newStore (insert envS newEnv locState))

--alloc (State state) ident type_ = State (insert envS newEnv (insert storeS newStore state2))
--  where
--    (loc, State state2) = nextLoc (State state)
--    newEnv = Env (insertAEnv oldEnv ident loc)
--      where
--        oldEnv = env (State state)
--    newStore = Store (insertAEnv oldStore loc (type_, Placeholder))
--      where
--        oldStore = store (State state)
assign :: State -> Ident -> Type -> Var -> State
assign (ErrorState state error) _ _ _ = (ErrorState state error)
assign (State state) ident type_ var =
  let loc = getJustLoc (State state) ident
   in let newStore = Store (rAssign (store (State state)) loc (type_, var))
       in State (insert storeS newStore state)

--   in let newStore = Store (insertAEnv (store (State state)) loc (type_, var))
rAssign :: AEnv Loc (Type, Var) -> Loc -> (Type, Var) -> AEnv Loc (Type, Var)
rAssign (x:xs) loc (type_, var) =
  case Data.Map.lookup loc x of
    Just (type2, var2) -> (insert loc (type_, var) x) : xs
    Nothing -> x : rAssign xs loc (type_, var)

typesFromTypesDecl :: [TypeDecl] -> [Type]
typesFromTypesDecl typeDecl = Prelude.map f typeDecl
  where
    f (TypeDecl type_ ident) = type_

resetShout :: State -> State
resetShout (ErrorState state error) = ErrorState state error
resetShout (State state) = State (insert msgS (Msg []) state)

addToShout :: State -> [String] -> State
addToShout (ErrorState state error) _ = (ErrorState state error)
addToShout (State state) str = State (insert msgS (Msg (str ++ (msg (State state)))) state)

shout :: State -> Var -> State
shout (ErrorState state error) _ = (ErrorState state error)
shout state (FVar _) = throwError state "functions cannot be shouted"
shout (State state) var = State (insert msgS (Msg (string : (msg (State state)))) state)
  where
    string =
      case var of
        IntV i -> show i
        StringV s -> show s
        BoolV b -> show b
        Placeholder -> "undeclared"

errState (ErrorState state _) = errState state
errState (State state) = state