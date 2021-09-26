module Expander (expand) where

import qualified Data.Map as Map

import Parse
import Error

type Ops = Map.Map Label Label

-- applies ops onto ast
expand :: Program -> [ModuleStmt] -> ModuleId -> Program
expand p ops mid = let
    ops' = foldr (\(OpDec op label) acc -> Map.insert op label acc) 
                 Map.empty ops
    in expandSeq p ops' mid

expandSeq :: Seq -> Ops -> ModuleId -> Seq
expandSeq (Seq []) ops mid = (Seq [])
expandSeq (Seq (s:ss)) ops mid = let
    s' = expandS s ops mid
    (Seq ss') = expandSeq (Seq ss) ops mid
    in Seq (s':ss')

expandS :: Stmt -> Ops -> ModuleId -> Stmt
expandS s@(NOP _) ops mid = s
expandS (Block ni ss) ops mid = Block ni $ expandSeq ss ops mid
expandS (Stmt ni x) ops mid = Stmt ni $ expandX x ops mid
expandS (If ni x ss1 ss2) ops mid = 
    If ni (expandX x ops mid) (expandSeq ss1 ops mid) (expandSeq ss2 ops mid)
expandS (Fn ni fl fps fss) ops mid = 
    Fn ni fl fps $ expandSeq fss ops mid
expandS dt@(DType _ _ _ _) ops mid = dt
expandS ta@(TypeAlias _ _ _) ops mid = ta
expandS (While ni x ss) ops mid = 
    While ni (expandX x ops mid) (expandSeq ss ops mid)

expandS (Case ni x pss) ops mid = 
    Case ni (expandX x ops mid) (aux pss ops mid)
 where
    aux [] ops mid = []
    aux ((px, ss):pes) ops mid = 
        (expandP px ops mid, expandSeq ss ops mid) : aux pes ops mid

expandS (Cond ni es) ops mid = 
    Cond ni (aux es ops mid)
 where
    aux [] ops mid = []
    aux ((x, ss):es) ops mid = 
        (expandX x ops mid, expandSeq ss ops mid) : aux es ops mid

expandS (Dec ni x) ops mid = Dec ni $ expandX x ops mid
expandS (DecAssign ni label x) ops mid = 
    DecAssign ni label $ expandX x ops mid
expandS (Assign ni x1 x2) ops mid = 
    Assign ni (expandX x1 ops mid) (expandX x2 ops mid)
expandS (Return ni x) ops mid = Return ni $ expandX x ops mid

expandX :: Expr -> Ops -> ModuleId -> Expr
expandX v@(Var _ _) ops mid = v
expandX v@(PInt _ _) ops mid = v
expandX v@(PFloat _ _) ops mid = v
expandX v@(PString _ _) ops mid = v
expandX v@(PChar _ _) ops mid = v
expandX v@(PBool _ _) ops mid = v
expandX v@(PVoid _) ops mid = v
expandX (PArray ni xs) ops mid = 
    PArray ni $ map (\x -> expandX x ops mid) xs
expandX (PTuple ni xs) ops mid = 
    PTuple ni $ map (\x -> expandX x ops mid) xs
expandX (PList ni xs) ops mid = 
    PList ni $ map (\x -> expandX x ops mid) xs
expandX (Ap ni x1 x2) ops mid = 
    Ap ni (expandX x1 ops mid) (expandX x2 ops mid)
expandX (ApNull ni x) ops mid = ApNull ni (expandX x ops mid)
expandX (IfX ni x1 x2 x3) ops mid = 
    IfX ni (expandX x1 ops mid) (expandX x2 ops mid) (expandX x3 ops mid)
expandX (Lambda ni ps ss) ops mid = 
    Lambda ni ps (expandSeq ss ops mid)
expandX (CaseX ni x1 pes) ops mid = 
    CaseX ni (expandX x1 ops mid) (aux pes ops mid)
 where
    aux [] ops mid = []
    aux ((px,x):pes) ops mid = 
        (expandP px ops mid, expandX x ops mid) : aux pes ops mid
expandX (CondX ni es) ops mid = 
    CondX ni (aux es ops mid)
 where
    aux [] ops mid = []
    aux ((x1,x2):es) ops mid = 
        (expandX x1 ops mid, expandX x2 ops mid) : aux es ops mid

-- here's where the magic happens
expandX (Op1 label ni x1) ops mid | label `Map.member` ops =
    Ap ni (Var ni (ops Map.! label)) (expandX x1 ops mid)
expandX (Op1 label ni x1) ops mid = Op1 label ni $ expandX x1 ops mid
expandX (Op2 label ni x1 x2) ops mid | label `Map.member` ops =
    Ap ni (Ap ni (Var ni (ops Map.! label))
    (expandX x1 ops mid)) (expandX x2 ops mid)
expandX (Op2 label ni x1 x2) ops mid = 
    Op2 label ni (expandX x1 ops mid) (expandX x2 ops mid)


expandP :: PatExpr -> Ops -> ModuleId -> PatExpr
expandP px@(PatVar _ _) ops mid = px
expandP px@(PatInt _ _) ops mid = px
expandP px@(PatFloat _ _) ops mid = px
expandP px@(PatString _ _) ops mid = px
expandP px@(PatChar _ _) ops mid = px
expandP px@(PatBool _ _) ops mid = px
expandP px@(PatVoid _) ops mid = px
expandP (PatArray ni pxs) ops mid = 
    PatArray ni $ map (\px -> expandP px ops mid) pxs
expandP (PatTuple ni pxs) ops mid = 
    PatTuple ni $ map (\px -> expandP px ops mid) pxs
expandP (PatList ni pxs) ops mid = 
    PatList ni $ map (\px -> expandP px ops mid) pxs
expandP (PatAp ni px1 px2) ops mid = 
    PatAp ni (expandP px1 ops mid) (expandP px2 ops mid)
expandP (AsPattern ni label px) ops mid = 
    AsPattern ni label (expandP px ops mid)
-- more magic
expandP (PatOp2 label ni px1 px2) ops mid | label `Map.member` ops = let
    px1' = expandP px1 ops mid
    px2' = expandP px2 ops mid
    trg  = PatVar ni $ ops Map.! label
    in
    PatAp ni (PatAp ni trg px1') px2'
expandP (PatOp2 label ni px1 px2) ops mid =
    error $ makeErrMsg ni mid 
        $ "Expander: Could not find op: `" ++ label ++ "`."
