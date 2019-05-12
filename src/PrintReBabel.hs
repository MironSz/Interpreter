{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module PrintReBabel where

-- pretty-printer generated by the BNF converter

import AbsReBabel
import Data.Char


-- the top-level printing method
printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : "," :ts -> showString t . space "," . rend i ts
    t  : ")" :ts -> showString t . showChar ')' . rend i ts
    t  : "]" :ts -> showString t . showChar ']' . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else (' ':s))

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- the printer class does the job
class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j<i then parenth else id


instance Print Integer where
  prt _ x = doc (shows x)


instance Print Double where
  prt _ x = doc (shows x)


instance Print Ident where
  prt _ (Ident i) = doc (showString ( i))
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])


instance Print Program where
  prt i e = case e of
    Program stmts -> prPrec i 0 (concatD [prt 0 stmts])

instance Print FFalse where
  prt i e = case e of
    FFalse -> prPrec i 0 (concatD [doc (showString "false")])

instance Print TTrue where
  prt i e = case e of
    TTrue -> prPrec i 0 (concatD [doc (showString "true")])

instance Print TrueInteger where
  prt i e = case e of
    Positive n -> prPrec i 0 (concatD [prt 0 n])
    Negative n -> prPrec i 0 (concatD [doc (showString "-"), prt 0 n])

instance Print RawCondition where
  prt i e = case e of
    TrueCond ttrue -> prPrec i 0 (concatD [prt 0 ttrue])
    FFalseCond ffalse -> prPrec i 0 (concatD [prt 0 ffalse])
    BExpr condition1 boperator condition2 -> prPrec i 0 (concatD [prt 0 condition1, prt 0 boperator, prt 0 condition2])
    Equal item1 item2 -> prPrec i 0 (concatD [prt 0 item1, doc (showString "=="), prt 0 item2])
    Greater item1 item2 -> prPrec i 0 (concatD [prt 0 item1, doc (showString ">"), prt 0 item2])
    Smaller item1 item2 -> prPrec i 0 (concatD [prt 0 item1, doc (showString "<"), prt 0 item2])
    Negate condition -> prPrec i 0 (concatD [doc (showString "not"), prt 0 condition])

instance Print Condition where
  prt i e = case e of
    Cond rawcondition -> prPrec i 0 (concatD [doc (showString "("), prt 0 rawcondition, doc (showString ")")])

instance Print Operator where
  prt i e = case e of
    DefineOperator type_1 id type_2 lambda -> prPrec i 0 (concatD [doc (showString "("), prt 0 type_1, doc (showString ")"), prt 0 id, doc (showString "("), prt 0 type_2, doc (showString ")"), doc (showString "="), prt 0 lambda])

instance Print Expr where
  prt i e = case e of
    CallOperator item1 operator item2 -> prPrec i 0 (concatD [prt 0 item1, prt 0 operator, prt 0 item2])
    ECall call -> prPrec i 0 (concatD [prt 0 call])
    MathExpr item1 mathop item2 -> prPrec i 0 (concatD [prt 0 item1, prt 0 mathop, prt 0 item2])

instance Print Stmt where
  prt i e = case e of
    Empty -> prPrec i 0 (concatD [doc (showString ";")])
    BlockStatement block -> prPrec i 0 (concatD [prt 0 block])
    Decl type_ id -> prPrec i 0 (concatD [prt 0 type_, prt 0 id])
    Ass id item -> prPrec i 0 (concatD [prt 0 id, doc (showString "="), prt 0 item])
    IfStmt condition block1 block2 -> prPrec i 0 (concatD [doc (showString "if"), prt 0 condition, prt 0 block1, doc (showString "else"), prt 0 block2])
    WhileStmt condition block -> prPrec i 0 (concatD [doc (showString "while"), prt 0 condition, prt 0 block])
    PrintStatement item -> prPrec i 0 (concatD [doc (showString "shout"), prt 0 item])
    CallStmt call -> prPrec i 0 (concatD [prt 0 call])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ";"), prt 0 xs])
instance Print Block where
  prt i e = case e of
    Block stmts -> prPrec i 0 (concatD [doc (showString "{"), prt 0 stmts, doc (showString "}")])

instance Print RBlock where
  prt i e = case e of
    ReturnBlock stmts rstmt -> prPrec i 0 (concatD [doc (showString "{"), prt 0 stmts, prt 0 rstmt, doc (showString "}")])

instance Print RStmt where
  prt i e = case e of
    RBlockStatement rblock -> prPrec i 0 (concatD [prt 0 rblock])
    Return item -> prPrec i 0 (concatD [doc (showString "return"), prt 0 item])
    VoidReturn -> prPrec i 0 (concatD [doc (showString "done")])
    RIfStmt condition rblock1 rblock2 -> prPrec i 0 (concatD [doc (showString "if"), prt 0 condition, prt 0 rblock1, doc (showString "else"), prt 0 rblock2])

instance Print Type where
  prt i e = case e of
    VoidT -> prPrec i 0 (concatD [doc (showString "void")])
    IntT -> prPrec i 0 (concatD [doc (showString "int")])
    BoolT -> prPrec i 0 (concatD [doc (showString "bool")])
    StringT -> prPrec i 0 (concatD [doc (showString "string")])
    FunctionT ftype -> prPrec i 0 (concatD [prt 0 ftype])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print FType where
  prt i e = case e of
    Function types type_ -> prPrec i 0 (concatD [doc (showString "function"), doc (showString "("), prt 0 types, doc (showString ")"), doc (showString "->"), doc (showString "("), prt 0 type_, doc (showString ")")])

instance Print Lambda where
  prt i e = case e of
    Lambda ids rblock -> prPrec i 0 (concatD [doc (showString "lambda"), prt 0 ids, doc (showString "->"), prt 0 rblock])

instance Print Item where
  prt i e = case e of
    ItemExpr expr -> prPrec i 0 (concatD [prt 0 expr])
    BracesItem item -> prPrec i 0 (concatD [doc (showString "("), prt 0 item, doc (showString ")")])
    ItemIdent id -> prPrec i 0 (concatD [prt 0 id])
    ItemLiteral trueinteger -> prPrec i 0 (concatD [prt 0 trueinteger])
    ItemString str -> prPrec i 0 (concatD [prt 0 str])
    ItemLambda lambda -> prPrec i 0 (concatD [prt 0 lambda])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print RefOrVal where
  prt i e = case e of
    Ref id -> prPrec i 0 (concatD [doc (showString "ref"), prt 0 id])
    Val item -> prPrec i 0 (concatD [doc (showString "val"), prt 0 item])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print Call where
  prt i e = case e of
    Call item reforvals -> prPrec i 0 (concatD [doc (showString "call"), prt 0 item, prt 0 reforvals])

instance Print BOperator where
  prt i e = case e of
    And -> prPrec i 0 (concatD [doc (showString "and")])
    Or -> prPrec i 0 (concatD [doc (showString "or")])
    Neither -> prPrec i 0 (concatD [doc (showString "neither")])
    Xor -> prPrec i 0 (concatD [doc (showString "xor")])

instance Print MathOp where
  prt i e = case e of
    Add -> prPrec i 0 (concatD [doc (showString "+")])
    Sub -> prPrec i 0 (concatD [doc (showString "-")])
    Mul -> prPrec i 0 (concatD [doc (showString "*")])
    Div -> prPrec i 0 (concatD [doc (showString "/")])


