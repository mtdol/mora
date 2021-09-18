module Expander (expand) where

import Parse
import qualified Data.Map as Map

type Ops = Map.Map Label Label

-- applies ops onto ast
expand :: Program -> [ModuleStmt] -> Program
expand p ops = let
    ops' = foldr (\(OpDec op label) acc -> Map.insert op label acc) 
                 Map.empty ops
    in expandSeq p ops'

expandSeq :: Seq -> Ops -> Seq
expandSeq (Seq []) ops = (Seq [])
expandSeq (Seq (s:ss)) ops = let
    s' = expandS s ops
    (Seq ss') = expandSeq (Seq ss) ops
    in Seq (s':ss')

expandS :: Stmt -> Ops -> Stmt
expandS NOP ops = NOP
expandS (Block ss) ops = Block $ expandSeq ss ops
expandS (Stmt x) ops = Stmt $ expandX x ops
expandS (If x ss1 ss2) ops = 
    If (expandX x ops) (expandSeq ss1 ops) (expandSeq ss2 ops)
expandS (Fn fl fps fss) ops = 
    Fn fl fps $ expandSeq fss ops
expandS dt@(DType _ _ _) ops = dt
expandS ta@(TypeAlias _ _) ops = ta
expandS (While x ss) ops = 
    While (expandX x ops) (expandSeq ss ops)

expandS (Case x pss) ops = Case (expandX x ops) (aux pss ops)
 where
    aux [] ops = []
    aux ((px, ss):pes) ops = 
        (expandP px ops, expandSeq ss ops) : aux pes ops

expandS (Dec x) ops = Dec $ expandX x ops
expandS (DecAssign label x) ops = DecAssign label $ expandX x ops
expandS (Assign x1 x2) ops = 
    Assign (expandX x1 ops) (expandX x2 ops)
expandS (Return x) ops = Return $ expandX x ops

expandX :: Expr -> Ops -> Expr
expandX v@(Var _) ops = v
expandX v@(PInt _) ops = v
expandX v@(PFloat _) ops = v
expandX v@(PString _) ops = v
expandX v@(PChar _) ops = v
expandX v@(PBool _) ops = v
expandX v@(PArray _) ops = v
expandX v@(PTuple _) ops = v
expandX v@(PVoid) ops = v
expandX (Ap x1 x2) ops = Ap (expandX x1 ops) (expandX x2 ops)
expandX (ApNull x) ops = ApNull (expandX x ops)
expandX (IfX x1 x2 x3) ops = 
    IfX (expandX x1 ops) (expandX x2 ops) (expandX x3 ops)
expandX (Lambda ps ss) ops = 
    Lambda ps (expandSeq ss ops)
expandX (CaseX x1 pes) ops = 
    CaseX (expandX x1 ops) (aux pes ops)
 where
    aux [] ops = []
    aux ((px,x):pes) ops = 
        (expandP px ops, expandX x ops) : aux pes ops

-- here's where the magic happens
expandX (Op1 label x1) ops | label `Map.member` ops =
    Ap (Var (ops Map.! label)) (expandX x1 ops)
expandX (Op1 label x1) ops = Op1 label $ expandX x1 ops
expandX (Op2 label x1 x2) ops | label `Map.member` ops =
    Ap (Ap (Var (ops Map.! label)) (expandX x1 ops)) (expandX x2 ops)
expandX (Op2 label x1 x2) ops = 
    Op2 label (expandX x1 ops) (expandX x2 ops)


expandP :: PatExpr -> Ops -> PatExpr
expandP px@(PatVar _) ops = px
expandP px@(PatInt _) ops = px
expandP px@(PatFloat _) ops = px
expandP px@(PatString _) ops = px
expandP px@(PatChar _) ops = px
expandP px@(PatBool _) ops = px
expandP px@(PatVoid) ops = px
expandP (PatArray pxs) ops = 
    PatArray $ map (\px -> expandP px ops) pxs
expandP (PatTuple pxs) ops = 
    PatTuple $ map (\px -> expandP px ops) pxs
expandP (PatAp px1 px2) ops = 
    PatAp (expandP px1 ops) (expandP px2 ops)
expandP (AsPattern label px) ops = 
    AsPattern label (expandP px ops)
-- more magic
expandP (PatOp2 label px1 px2) ops | label `Map.member` ops = let
    px1' = expandP px1 ops
    px2' = expandP px2 ops
    trg  = PatVar $ ops Map.! label
    in
    PatAp (PatAp trg px1') px2'
expandP (PatOp2 label px1 px2) ops =
    error $ "Expander: Could not find op: `" ++ label ++ "`."
