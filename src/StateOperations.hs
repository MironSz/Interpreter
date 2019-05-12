module StateOperations where


type Loc  = Int
data Env = Env Map Ident (Loc,Type) | NestedEnv Env Env
type Store =  Map Loc Var

data Var = Int Int | Bool Bool |  String String | FType FType

data State = State Store Env [StateVal]

data StateVal = Error String | MinLoc Int


emptyState :: State
emptyState = State empty  (Env empty)  [0]

nest :: State->State
nest (State store env stateVals) = State store (NestedEnv (Env empty) env)  stateVals

unnest :: State -> State
unnest (State s (NestedEnv e1 e2) stateVals) = State s e2 stateVals

getLoc :: Env->Ident->Maybe (Loc,Type)
getLoc (Env m)  id = lookup id m

getVarFromLoc :: Store->Maybe Loc ->Maybe (Var,Type)
getVarFromLoc s (Just (l,t)) = ((lookup l s),t)

getVar :: State ->Ident-> Maybe Var
getVar (State s env err) id  = getVarFromLoc s l where l = getLoc env id

incMinLoc :: [StateVals] -> [StateVals]
incMinLoc (MinLoc i):xs -> (MinLoc (i+1)):xs

nextLoc :: [StateVals] -> Loc
nextLoc (MinLoc loc):xs -> Loc loc

throwError :: [StateVal]->String->[StateVal]
throwError sv s = sv ++ [Error s]


addToEnv :: Env->Loc->Ident ->Type->Env
addToEnv (Env m) loc ident t = insert ident (loc,t)
