module Desugar (desugar) where

import Parse
import Error
import Utilities (gatherFns)

desugar :: Program -> ModuleId -> Program
desugar p mid = let
    p'   = simpleDecs p 
    p''  = scope p' mid
    p''' = liftFns p'' mid
    in p'''

-- relabels local variables such that scoped and shadowed variables have
--  unique names
--
--  ex: `{dec a; a; {dec a; a} a;}` -> {dec a'; a'; {dec a''; a''} a';}
scope :: Program -> ModuleId -> Program
scope p mid = let
    ((Seq ss'), _) = aux 0 True p mid
    in Seq ss'
 where
    --     unique id  top level?  prog   
    aux :: Int ->     Bool ->     Seq  -> ModuleId
    --     relabeled prog   curr id
        -> (Seq,            Int)
    aux i tl (Seq []) mid = (Seq [], i)
    -- relabel everything after this to the new label
    aux i False (Seq ((Dec ni (Var vni ol)):ss)) mid = let
        nl = getNewLabel ol i
        s = Dec ni (Var vni nl)
        ss' = relabel False (Seq ss) ol nl
        ((Seq ss''),i') = aux (i+1) False ss' mid
        in (Seq (s:ss''),i')
    -- also for DecAssign
    aux i False (Seq ((DecAssign ni ol x):ss)) mid = let
        nl = getNewLabel ol i
        s = DecAssign ni nl x
        ss' = relabel False (Seq ss) ol nl
        ((Seq ss''),i') = aux (i+1) False ss' mid
        in (Seq (s:ss''),i')
    -- general case
    aux i tl (Seq (s:ss)) mid = let
        (s',i') = auxS i s mid
        ((Seq ss'),i'') = aux i' tl (Seq ss) mid
        in (Seq (s':ss'),i'')
    
    auxS :: Int -> Stmt -> ModuleId -> (Stmt,Int)
    auxS i s@(NOP _) mid = (s,i)
    auxS i (Block ni ss) mid = let 
        (ss',i') = aux i False ss mid
        in (Block ni ss',i')
    auxS i s@(Stmt _ _) mid = (s,i)
    auxS i (If ni x ss1 ss2) mid = let
        (ss1',i') = aux i False ss1 mid
        (ss2',i'') = aux i' False ss2 mid
        in (If ni x ss1' ss2',i'')
    auxS i (Fn ni fl fps fss) mid = let
        (fss',i') = aux i False fss mid
        in (Fn ni fl fps fss',i')
    auxS i s@(DType _ _ _ _) mid = (s,i)
    auxS i s@(TypeAlias _ _ _) mid = (s,i)
    auxS i (While ni x ss) mid = let
        (ss',i') = aux i False ss mid
        in (While ni x ss',i')
    auxS i (Case ni x es) mid = let
        caseAux i [] = ([],i)
        caseAux i ((px,ss):es) = let
            (ss',i') = aux i False ss mid
            (es',i'') = caseAux i' es
            in ((px,ss'):es',i'')
        (es',i') = caseAux i es
        in (Case ni x es',i')
    auxS i s@(Dec ni x) mid = (s,i)
    auxS i s@(DecAssign ni label x) mid = (s,i)
    auxS i s@(Assign ni x1 x2) mid = (s,i)
    auxS i s@(Return ni x) mid = (s,i)

    getNewLabel label i = "s" ++ show i ++ ":" ++ label

-- moves all lambdas and fns to the top-level and 
--  renames all references to these functions 
liftFns :: Seq -> ModuleId -> Seq
liftFns ss mid = let 
    ((Seq ss'), fns, _) = aux 0 True ss [] mid
    -- concat fns to the front of the toplevel
    in Seq (fns++ss')
 where
    --     unique id  top level?  prog    fns
    aux :: Int ->     Bool ->     Seq  -> [Stmt] -> ModuleId
    --     relabeled prog    gathered fns  curr id
        -> (Seq,             [Stmt],       Int)
    aux i tl (Seq []) fns mid = (Seq [], fns, i)
    aux i tl (Seq ((Fn ni fl fps fss):ss)) fns mid = let
        -- allows all of the functions to access each other through relabeling
        doRelabel :: [Stmt] -> Seq -> Int -> ([Stmt],Seq,Int)
        doRelabel ffns ss i = foldr
            (\(Fn _ ol _ _) (ffns,ss,i) -> let
                nl = getNewLabel ol i
                -- relabel every function
                ffns' = map 
                    (\(Fn ni fl fps fb) -> let
                        -- make sure that we also update our own name!
                        fl' = if fl == ol then nl else fl
                        in Fn ni fl' fps (relabel True fb ol nl)) 
                    ffns
                -- relabel the main body
                ss' = relabel True ss ol nl
                in (ffns',ss',i+1)
            )
            (ffns,ss,i) ffns
        -- gather all functions from fss (removes them from `fss`)
        (ffns,fss') = gatherFns fss
        (ffns',fss'',i') = doRelabel ffns fss' i
        -- now generally process the fn body
        (fss''',fns',i'') = aux i' False fss'' fns mid
        s' = Fn ni fl fps fss'''
        (Seq ss',fns'',i''') = aux i'' tl (Seq ss) fns' mid
        -- now run `aux` on every one of the ffns to deal
        --  with sub-procs inside of sub-procs
        ((Seq ffns''),fns''',i'''') = aux i''' False (Seq ffns') fns'' mid
        in (Seq (s':ss'),ffns''++fns''',i'''')
    -- general case
    aux i tl (Seq (s:ss)) fns mid = let
        (s',fns',i') = auxS i s fns mid
        ((Seq ss'),fns'',i'') = aux i' tl (Seq ss) fns' mid
        in (Seq (s':ss'),fns'',i'')
    
    auxS :: Int -> Stmt -> [Stmt] -> ModuleId -> (Stmt,[Stmt],Int)
    auxS i s@(NOP _) fns mid = (s,fns,i)
    auxS i (Block ni ss) fns mid = let 
        (ss',fns',i') = aux i False ss fns mid
        in (Block ni ss',fns',i')
    auxS i s@(Stmt _ _) fns mid = (s,fns,i)
    auxS i (If ni x ss1 ss2) fns mid = let
        (ss1',fns',i') = aux i False ss1 fns mid
        (ss2',fns'',i'') = aux i' False ss2 fns' mid
        in (If ni x ss1' ss2', fns'',i'')
    auxS i (Fn ni fl fps fss) fns mid = 
        error $ makeErrMsg ni mid $
            "Something went wrong. We shouldn't have a function here."
    -- auxS i (Fn ni fl fps fss) fns mid = let
    --     (fss',fns',i') = aux i False fss fns mid
    --     in (Fn ni fl fps fss',fns',i')
    auxS i s@(DType _ _ _ _) fns mid = (s,fns,i)
    auxS i s@(TypeAlias _ _ _) fns mid = (s,fns,i)
    auxS i (While ni x ss) fns mid = let
        (ss',fns',i') = aux i False ss fns mid
        in (While ni x ss', fns',i')
    auxS i (Case ni x es) fns mid = let
        caseAux i [] fns = ([],fns,i)
        caseAux i ((px,ss):es) fns = let
            (ss',fns',i') = aux i False ss fns mid
            (es',fns'',i'') = caseAux i' es fns'
            in ((px,ss'):es',fns'',i'')
        (es',fns',i') = caseAux i es fns
        in (Case ni x es',fns',i')
    auxS i s@(Dec ni x) fns mid = (s,fns,i)
    auxS i s@(DecAssign ni label x) fns mid = (s,fns,i)
    auxS i s@(Assign ni x1 x2) fns mid = (s,fns,i)
    auxS i s@(Return ni x) fns mid = (s,fns,i)

    getNewLabel label i = "fn" ++ show i ++ ":" ++ label

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

-- Relabels all instances of old label with new label until we
--  see a function definition or we see a redeclaration of the old label
--
--         cut through functions?  program   old label  new label
relabel :: Bool ->                 Seq ->    Label ->   Label 
--     resutling program
    -> Seq
relabel cut (Seq []) _ _ = (Seq [])
-- stop when we see a declaration with the same name
relabel cut (Seq (s@(Dec ni (Var vni label)):ss)) ol nl | label == ol =
    Seq (s:ss)
-- ditto
relabel cut (Seq (s@(DecAssign ni label x):ss)) ol nl | label == ol = let
    -- we still relabel the rhs of DecAssign to allow access to an old
    --  definition of the lhs label
    x' = relabelX cut x ol nl
    s' = DecAssign ni label x'
    in Seq (s':ss)
-- general case
relabel cut (Seq (s:ss)) ol nl = let
    s'  = relabelS cut s ol nl
    (Seq ss') = relabel cut (Seq ss) ol nl
    in (Seq (s':ss'))

relabelS :: Bool -> Stmt -> Label -> Label -> Stmt
relabelS cut s@(NOP _) ol nl = s
relabelS cut (Block ni ss) ol nl = Block ni (relabel cut ss ol nl)
relabelS cut (Stmt ni x) ol nl = Stmt ni (relabelX cut x ol nl)
relabelS cut (If ni x ss1 ss2) ol nl = 
    If ni (relabelX cut x ol nl) (relabel cut ss1 ol nl) (relabel cut ss2 ol nl)
relabelS cut dt@(DType _ _ _ _) ol nl = dt
relabelS cut ta@(TypeAlias _ _ _) ol nl = ta
relabelS cut (While ni x ss) ol nl = 
    While ni (relabelX cut x ol nl) (relabel cut ss ol nl)
relabelS cut (Case ni x elems) ol nl = let
    elems' = map (\(px, ss) -> (px, relabel cut ss ol nl)) elems
    x' = relabelX cut x ol nl
    in Case ni x' elems'
relabelS cut s@(Dec ni x) ol nl = s
relabelS cut (DecAssign ni label x) ol nl = 
    (DecAssign ni label (relabelX cut x ol nl))
relabelS cut (Assign ni x1 x2) ol nl = 
    Assign ni (relabelX cut x1 ol nl) (relabelX cut x2 ol nl)
relabelS cut (Return ni x) ol nl = 
    Return ni (relabelX cut x ol nl)

relabelS False s@(Fn ni label fps ss2) ol nl = s
relabelS True (Fn ni label fps fss) ol nl = 
    Fn ni label fps (relabel True fss ol nl)

--relabelS s _ _ = error $ "Desugar->relabel: Unknown stmt:\n" ++ show s


relabelX :: Bool -> Expr -> Label -> Label -> Expr
relabelX cut (Var ni label) ol nl | label == ol = (Var ni nl)
relabelX cut (Var ni label) ol nl = (Var ni label)
relabelX cut x@(PInt _ _) _ _ = x
relabelX cut x@(PFloat _ _) _ _ = x
relabelX cut x@(PBool _ _) _ _ = x
relabelX cut x@(PString _ _) _ _ = x
relabelX cut x@(PChar _ _) _ _ = x
relabelX cut x@(PVoid _) _ _ = x

relabelX cut x@(PArray _ _) _ _ = x
relabelX cut x@(PTuple _ _) _ _ = x
relabelX cut (Ap ni x1 x2) ol nl = 
    Ap ni (relabelX cut x1 ol nl) (relabelX cut x2 ol nl)
relabelX cut (ApNull ni x) ol nl = ApNull ni (relabelX cut x ol nl)
relabelX cut (IfX ni x1 x2 x3) ol nl = 
    IfX ni 
    (relabelX cut x1 ol nl) (relabelX cut x2 ol nl) (relabelX cut x3 ol nl) 
relabelX cut (CaseX ni x elems) ol nl = let
    elems' = map (\(px, x) -> (px, relabelX cut x ol nl)) elems
    x' = relabelX cut x ol nl
    in CaseX ni x' elems'
relabelX cut (Op2 op ni x1 x2) ol nl = 
    Op2 op ni (relabelX cut x1 ol nl) (relabelX cut x2 ol nl)
relabelX cut (Op1 op ni x) ol nl = 
    Op1 op ni (relabelX cut x ol nl)

relabelX False x@(Lambda ni _ _) _ _ = x
relabelX True (Lambda ni lps lss) ol nl = 
    Lambda ni lps (relabel True lss ol nl)

--relabelX x _ _ = error $ "Desugar->relabel: Unknown expr:\n" ++ show x
