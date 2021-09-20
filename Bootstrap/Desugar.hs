module Desugar (desugar) where

import Parse
import Error

desugar :: Program -> Program
desugar p = let
    p'  = simpleDecs p 
    p'' = liftFns p' 0
    in p''

-- moves all lambdas and fns to the top-level and 
--  renames all references to these functions 
liftFns :: Seq -> Int -> Seq
liftFns ss i = ss

-- simplifies `dec n, x :: t, y;` into 
--  `dec n; dec x; x :: t; dec y;`
simpleDecs :: Seq -> Seq
simpleDecs ss@(Seq []) = ss
simpleDecs (Seq ((Dec ni x):ss)) = let
    decs = aux x
    (Seq ss') = simpleDecs (Seq ss)
    in Seq (decs ++ ss')
 where
    aux :: Expr -> [Stmt]
    aux x@(Var ni label) = [Dec ni x]
    aux x@(Op2 "::" ni (Var vni label) tx) = 
        [Dec ni (Var vni label), Stmt ni x]
    aux (Op2 "," ni x1 x2) = aux x1 ++ aux x2
simpleDecs (Seq ((Block ni ss1):ss)) = let 
    s'  = Block ni (simpleDecs ss1)
    (Seq ss') = simpleDecs (Seq ss)
    in Seq $ s' : ss'
simpleDecs (Seq ((If ni x ss1 ss2):ss)) = let 
    s'  = If ni x (simpleDecs ss1) (simpleDecs ss2)
    (Seq ss') = simpleDecs (Seq ss)
    in Seq $ s' : ss'
simpleDecs (Seq ((Fn ni flabel fps fss):ss)) = let 
    s'  = Fn ni flabel fps (simpleDecs fss)
    (Seq ss') = simpleDecs (Seq ss)
    in Seq $ s' : ss'
simpleDecs (Seq ((While ni x ss1):ss)) = let 
    s'  = While ni x (simpleDecs ss1)
    (Seq ss') = simpleDecs (Seq ss)
    in Seq $ s' : ss'
simpleDecs (Seq ((Case ni x elems):ss)) = let 
    elems' = map (\(px, ss) -> (px, simpleDecs ss)) elems
    s' = Case ni x elems'
    (Seq ss') = simpleDecs (Seq ss)
    in Seq $ s' : ss'
simpleDecs (Seq (s:ss)) = let
    (Seq ss') = simpleDecs (Seq ss)
    in (Seq (s : ss'))



--         program   old label  new label  resulting program
relabel :: Seq ->    Label ->   Label ->   Seq
relabel (Seq []) _ _ = (Seq [])
relabel (Seq (s:ss)) ol nl = let
    s'  = relabelS s ol nl
    (Seq ss') = relabel (Seq ss) ol nl
    in (Seq (s':ss'))

relabelS :: Stmt -> Label -> Label -> Stmt
relabelS s@(NOP _) ol nl = s
relabelS (Block ni ss) ol nl = Block ni (relabel ss ol nl)
relabelS (Stmt ni x) ol nl = Stmt ni (relabelX x ol nl)
relabelS (If ni x ss1 ss2) ol nl = 
    If ni (relabelX x ol nl) (relabel ss1 ol nl) (relabel ss2 ol nl)
relabelS dt@(DType _ _ _ _) ol nl = dt
relabelS ta@(TypeAlias _ _ _) ol nl = ta
relabelS (While ni x ss) ol nl = 
    While ni (relabelX x ol nl) (relabel ss ol nl)
relabelS (Case ni x elems) ol nl = let
    elems' = map (\(px, ss) -> (px, relabel ss ol nl)) elems
    x' = relabelX x ol nl
    in Case ni x' elems'
relabelS (Dec ni x) ol nl = (Dec ni x)
relabelS (DecAssign ni label x) ol nl = 
    (DecAssign ni label (relabelX x ol nl))
relabelS (Assign ni x1 x2) ol nl = 
    Assign ni (relabelX x1 ol nl) (relabelX x2 ol nl)
relabelS (Return ni x) ol nl = 
    Return ni (relabelX x ol nl)

relabelS (Fn ni label ss1 ss2) ol nl = 
    error $ makeErrMsgNI ni $ "Desugar->relabel: Cannot relabel fn."
--relabelS s _ _ = error $ "Desugar->relabel: Unknown stmt:\n" ++ show s


relabelX :: Expr -> Label -> Label -> Expr
relabelX (Var ni label) ol nl | label == ol = (Var ni nl)
relabelX (Var ni label) ol nl = (Var ni label)
relabelX x@(PInt _ _) _ _ = x
relabelX x@(PFloat _ _) _ _ = x
relabelX x@(PBool _ _) _ _ = x
relabelX x@(PString _ _) _ _ = x
relabelX x@(PChar _ _) _ _ = x
relabelX x@(PVoid _) _ _ = x

relabelX x@(PArray _ _) _ _ = x
relabelX x@(PTuple _ _) _ _ = x
relabelX (Ap ni x1 x2) ol nl = Ap ni (relabelX x1 ol nl) (relabelX x2 ol nl)
relabelX (ApNull ni x) ol nl = ApNull ni (relabelX x ol nl)
relabelX (IfX ni x1 x2 x3) ol nl = 
    IfX ni (relabelX x1 ol nl) (relabelX x2 ol nl) (relabelX x3 ol nl) 
relabelX (CaseX ni x elems) ol nl = let
    elems' = map (\(px, x) -> (px, relabelX x ol nl)) elems
    x' = relabelX x ol nl
    in CaseX ni x' elems'
relabelX (Op2 op ni x1 x2) ol nl = 
    Op2 op ni (relabelX x1 ol nl) (relabelX x2 ol nl)
relabelX (Op1 op ni x) ol nl = 
    Op1 op ni (relabelX x ol nl)

relabelX (Lambda ni _ _) _ _ = 
    error $ "Desugar->relabel: Cannot relabel Lambdas."
--relabelX x _ _ = error $ "Desugar->relabel: Unknown expr:\n" ++ show x
