module Desugar (desugar) where

import Parse

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
simpleDecs (Seq ((Dec x):ss)) = let
    decs = aux x
    (Seq ss') = simpleDecs (Seq ss)
    in Seq (decs ++ ss')
 where
    aux :: Expr -> [Stmt]
    aux x@(Var label) = [Dec x]
    aux x@(Op2 "::" (Var label) tx) = [Dec (Var label), Stmt x]
    aux (Op2 "," x1 x2) = aux x1 ++ aux x2
simpleDecs (Seq ((Block ss1):ss)) = let 
    s'  = Block (simpleDecs ss1)
    (Seq ss') = simpleDecs (Seq ss)
    in Seq $ s' : ss'
simpleDecs (Seq ((If x ss1 ss2):ss)) = let 
    s'  = If x (simpleDecs ss1) (simpleDecs ss2)
    (Seq ss') = simpleDecs (Seq ss)
    in Seq $ s' : ss'
simpleDecs (Seq ((Fn flabel fps fss):ss)) = let 
    s'  = Fn flabel fps (simpleDecs fss)
    (Seq ss') = simpleDecs (Seq ss)
    in Seq $ s' : ss'
simpleDecs (Seq ((While x ss1):ss)) = let 
    s'  = While x (simpleDecs ss1)
    (Seq ss') = simpleDecs (Seq ss)
    in Seq $ s' : ss'
simpleDecs (Seq ((Case x elems):ss)) = let 
    elems' = map (\(px, ss) -> (px, simpleDecs ss)) elems
    s' = Case x elems'
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
relabelS NOP ol nl = NOP
relabelS (Block ss) ol nl = Block (relabel ss ol nl)
relabelS (Stmt x) ol nl = Stmt (relabelX x ol nl)
relabelS (If x ss1 ss2) ol nl = 
    If (relabelX x ol nl) (relabel ss1 ol nl) (relabel ss2 ol nl)
relabelS dt@(DType _ _ _) ol nl = dt
relabelS ta@(TypeAlias _ _) ol nl = ta
relabelS (While x ss) ol nl = While (relabelX x ol nl) (relabel ss ol nl)
relabelS (Case x elems) ol nl = let
    elems' = map (\(px, ss) -> (px, relabel ss ol nl)) elems
    x' = relabelX x ol nl
    in Case x' elems'
relabelS (Dec x) ol nl = (Dec x)
relabelS (DecAssign label x) ol nl = 
    (DecAssign label (relabelX x ol nl))
relabelS (Assign x1 x2) ol nl = 
    (Assign (relabelX x1 ol nl) (relabelX x2 ol nl))
relabelS (Return x) ol nl = (Return (relabelX x ol nl))

relabelS (Fn label ss1 ss2) ol nl = 
    error $ "Desugar->relabel: Cannot relabel fn."
--relabelS s _ _ = error $ "Desugar->relabel: Unknown stmt:\n" ++ show s


relabelX :: Expr -> Label -> Label -> Expr
relabelX (Var label) ol nl | label == ol = (Var nl)
relabelX (Var label) ol nl = (Var label)
relabelX x@(PInt _) _ _ = x
relabelX x@(PFloat _) _ _ = x
relabelX x@(PString _) _ _ = x
relabelX x@(PChar _) _ _ = x
relabelX x@(PVoid) _ _ = x

relabelX x@(PArray _) _ _ = x
relabelX x@(PTuple _) _ _ = x
relabelX (Ap x1 x2) ol nl = Ap (relabelX x1 ol nl) (relabelX x2 ol nl)
relabelX (ApNull x) ol nl = ApNull (relabelX x ol nl)
relabelX (IfX x1 x2 x3) ol nl = 
    IfX (relabelX x1 ol nl) (relabelX x2 ol nl) (relabelX x3 ol nl) 
relabelX (CaseX x elems) ol nl = let
    elems' = map (\(px, x) -> (px, relabelX x ol nl)) elems
    x' = relabelX x ol nl
    in CaseX x' elems'
relabelX (Op2 op x1 x2) ol nl = 
    Op2 op (relabelX x1 ol nl) (relabelX x2 ol nl)
relabelX (Op1 op x) ol nl = 
    Op1 op (relabelX x ol nl)

relabelX (Lambda _ _) _ _ = 
    error $ "Desugar->relabel: Cannot relabel Lambdas."
relabelX x _ _ = error $ "Desugar->relabel: Unknown expr:\n" ++ show x
