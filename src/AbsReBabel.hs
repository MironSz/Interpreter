

module AbsReBabel where

-- Haskell module generated by the BNF converter




newtype Ident = Ident String deriving (Eq, Ord, Show, Read)
data Program = Program [Stmt]
  deriving (Eq, Ord, Show, Read)

data FFalse = FFalse
  deriving (Eq, Ord, Show, Read)

data TTrue = TTrue
  deriving (Eq, Ord, Show, Read)

data TrueInteger = Positive Integer | Negative Integer
  deriving (Eq, Ord, Show, Read)

data RawCondition
    = TrueCond TTrue
    | FFalseCond FFalse
    | BExpr Condition BOperator Condition
    | Equal Item Item
    | Greater Item Item
    | Smaller Item Item
    | Negate Condition
  deriving (Eq, Ord, Show, Read)

data Condition = Cond RawCondition
  deriving (Eq, Ord, Show, Read)

data Operator = DefineOperator Type Ident Type Lambda
  deriving (Eq, Ord, Show, Read)

data Expr
    = CallOperator Item Operator Item
    | ECall Call
    | MathExpr Item MathOp Item
  deriving (Eq, Ord, Show, Read)

data Stmt
    = Empty
    | BlockStatement Block
    | Decl Type Ident
    | Ass Ident Item
    | IfStmt Condition Block Block
    | WhileStmt Condition Block
    | PrintStatement Item
    | CallStmt Call
  deriving (Eq, Ord, Show, Read)

data Block = Block [Stmt]
  deriving (Eq, Ord, Show, Read)

data RBlock = ReturnBlock [Stmt] RStmt
  deriving (Eq, Ord, Show, Read)

data RStmt
    = RBlockStatement RBlock
    | Return Item
    | VoidReturn
    | RIfStmt Condition RBlock RBlock
  deriving (Eq, Ord, Show, Read)

data Type = VoidT | IntT | BoolT | StringT | FunctionT FType | Whatever
  deriving (Eq, Ord, Show, Read)

data FType = Function [Type] Type
  deriving (Eq, Ord, Show, Read)

data TypeDecl = TypeDecl Type Ident
  deriving (Eq, Ord, Show, Read)

data Lambda = Lambda [TypeDecl] Type RBlock
  deriving (Eq, Ord, Show, Read)

data Item
    = ItemExpr Expr
    | BracesItem Item
    | ItemIdent Ident
    | ItemLiteral TrueInteger
    | ItemString String
    | ItemLambda Lambda
  deriving (Eq, Ord, Show, Read)

data RefOrVal = Ref Ident | Val Item
  deriving (Eq, Ord, Show, Read)

data Call = Call Item [RefOrVal]
  deriving (Eq, Ord, Show, Read)

data BOperator = And | Or | Neither | Xor
  deriving (Eq, Ord, Show, Read)

data MathOp = Add | Sub | Mul | Div
  deriving (Eq, Ord, Show, Read)

