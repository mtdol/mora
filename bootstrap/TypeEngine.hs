{- This will eventually become the type engine.
 -  This code was left over from `WellFormed.hs`
 -  and exemplifies the structure of the AST. -}

module TypeEngine
(
wellFormed
) where

import Parse
import qualified Data.Set as Set
import Interp (preloadedLabels)


--
-- Utilites
--

type Defs = Set.Set Label
type Decs = Set.Set Label
--               local       global
type VarState = ((Decs,Defs),(Decs,Defs))

emptyDecs = Set.empty
emptyDefs = Set.empty

emptyVarState = ((emptyDecs,emptyDefs),(emptyDecs,emptyDefs))

accept :: Either String Bool
accept = Right True
reject :: String -> Either String Bool
reject msg = Left msg

check :: Label -> VarState -> Either String Bool
check label m =
    if defined label m 
        then accept
    else if declared label m
        then reject $ "Var `" ++ label ++"` is undefined."
    else
        reject $ "Var `" ++ label ++"` is undeclared."

declared :: Label -> VarState -> Bool
declared label ((ldecs,_),(gdecs,_)) = 
    label `Set.member` gdecs || label `Set.member` ldecs

declaredLocal :: Label -> VarState -> Bool
declaredLocal label ((ldecs,_),(_,_)) = 
    label `Set.member` ldecs 

defined :: Label -> VarState -> Bool
defined label ((_,ldefs),(_,gdefs)) = 
    label `Set.member` gdefs || label `Set.member` ldefs

-- Note: according to language rules, 
--  removes the label's binding from the local defs
--  (the variable is cleared after a new declaration)
declareLocal :: Label -> VarState -> VarState
declareLocal label ((ldecs,ldefs),(gdecs,gdefs)) = let
    ldecs' = label `Set.insert` ldecs
    ldefs' = label `Set.delete` ldefs
    in ((ldecs',ldefs'),(gdecs,gdefs))

declareGlobal :: Label -> VarState -> VarState
declareGlobal label ((ldecs,ldefs),(gdecs,gdefs)) = let
    gdecs' = label `Set.insert` gdecs
    in ((ldecs,ldefs),(gdecs',gdefs))

-- defines and declares a local var simultaniously
defineLocal :: Label -> VarState -> VarState
defineLocal label ((ldecs,ldefs),(gdecs,gdefs)) = let
    ldefs' = label `Set.insert` ldefs
    ldecs' = label `Set.insert` ldecs
    in ((ldecs',ldefs'),(gdecs,gdefs))
    
-- defines and declares a global var simultaniously
defineGlobal :: Label -> VarState -> VarState
defineGlobal label ((ldecs,ldefs),(gdecs,gdefs)) = let
    gdefs' = label `Set.insert` gdefs
    gdecs' = label `Set.insert` gdecs
    in ((ldecs,ldefs),(gdecs',gdefs'))

-- gets all labels from a `Dec` statement
-- ignore type declarations
getDecLabels (Op2 "::" x _) = getDecLabels x
getDecLabels (Op2 "," x1 x2) = getDecLabels x1 ++ getDecLabels x2
getDecLabels (Var label) = [label]

getDuplicates xs = aux xs Set.empty
 where
    aux [] _ = []
    aux (x:xs) m | x `Set.member` m = x : aux xs m
    aux (x:xs) m = aux xs (x `Set.insert` m)


--
-- Main
--

-- returns `Left errMsg` if the Expr is not well formed, else `Right True`
wellFormed :: Program -> Either String Bool
wellFormed p = do
    -- no duplicate var declarations in a block
    dupDecs p emptyDefs
    -- all vars must be declared and defined before use
    vars p

vars :: Program -> Either String Bool
vars p = do
    -- globaly define preloaded labels
    let m = foldr (\label acc -> defineGlobal label acc) emptyVarState
            $ map (\(label,_) -> label) preloadedLabels
    -- check definitions
    m'  <- varsDefs p m
    -- check the top-level
    varsSeq True p m''

-- `Left errMsg` if there are duplicate declarations, otherwise `Right True`
dupDecs :: Seq -> Defs -> Either String Bool
dupDecs (Seq ((Dec x):ss)) m = do
    let labels = getDecLabels x
    -- no duplicates allowed
    let dups = getDuplicates labels
    if not $ null dups 
        then genDupErr $ head dups
        else accept
    contains m labels
    let m' = foldr Set.insert m labels
    dupDecs (Seq ss) m'
 where 
    contains :: Defs -> [Label] -> Either String Bool
    contains _ [] = accept
    contains m (label:labels) = 
        if label `Set.member` m 
            then genDupErr label else contains m labels
dupDecs (Seq ((DecAssign label _):ss)) m =
    if label `Set.member` m then
        genDupErr label
    else dupDecs (Seq ss) (label `Set.insert` m)
dupDecs (Seq ((Block ss1):ss)) m = do 
    dupDecs ss1 emptyDefs
    dupDecs (Seq ss) m
dupDecs (Seq ((If _ ss1 ss2):ss)) m = do 
    dupDecs ss1 emptyDefs
    dupDecs ss2 emptyDefs
    dupDecs (Seq ss) m
dupDecs (Seq ((While _ ss1):ss)) m = do 
    dupDecs ss1 emptyDefs
    dupDecs (Seq ss) m
-- ok if the function label is redefined in the function body
dupDecs (Seq ((Fn flabel fps fss):ss)) m = do
    let dups = getDuplicates fps
    if not $ null dups 
        then reject $ "Duplicate function parameter `" ++ head dups 
            ++ "` for function `" ++ flabel ++ "`."
        else accept
    dupDecs fss (Set.fromList fps)
    dupDecs (Seq ss) m
dupDecs (Seq (_:ss)) m = dupDecs (Seq ss) m
dupDecs (Seq []) _ = accept      
 where
genDupErr label = 
    reject $ "Duplicate declaration for `" ++ label ++ "`."





-- binds all of the symbols available in the top-level
--  to the global context
--
-- binds: 
--  - `data` definitions
--  - `fn` defs
--  - `op` defs
--  - Declaration assignments (like `dec x <- 3`)
--
varsDefs :: Seq -> VarState -> Either String VarState
varsDefs (Seq []) m = Right m
varsDefs (Seq ((Fn flabel _ _):ss)) m = 
    varsDefs (Seq ss) (defineGlobal flabel m)
varsDefs (Seq ((DType _ _ elems):ss)) m = do
    let m' = aux elems m
    varsDefs (Seq ss) m'
 where
    aux :: [DTypeElem] -> VarState -> VarState
    aux [] m = m
    aux  ((cons, rhs):elems) m = 
        aux elems $ aux2 rhs $ defineGlobal cons m
    aux2 :: [((Maybe String, Maybe String), Expr)] -> VarState -> VarState 
    aux2 [] m = m
    aux2 (((Nothing,Nothing),_):elems) m = aux2 elems m
    aux2 (((Just get,Nothing),_):elems) m = aux2 elems (defineGlobal get m)
    aux2 (((Nothing,Just set),_):elems) m = aux2 elems (defineGlobal set m)
    aux2 (((Just get,Just set),_):elems) m = 
        aux2 elems (defineGlobal get (defineGlobal set m))
varsDefs (Seq ((Op op _):ss)) m = 
    varsDefs (Seq ss) (defineGlobal op m)
varsDefs (Seq ((DecAssign label _):ss)) m = 
    varsDefs (Seq ss) (defineGlobal label m)
varsDefs (Seq (_:ss)) m = varsDefs (Seq ss) m
    

-- checks that all vars in the program are defined
--
--         top-level?    program  memory      result
varsSeq :: Bool ->       Seq ->   VarState -> Either String Bool
varsSeq _ (Seq []) m = accept
varsSeq True (Seq ((Dec _):ss)) =
    reject $ "All top-level declarations must feature assignments."
varsSeq False (Seq ((Dec x) : ss)) m = do
    let m' = aux False x m
    varsSeq False (Seq ss) m'
 where
    aux x m = let
        labels = getDecLabels x
        in decLabels labels m
    decLabels [] m = m
    decLabels (label:labels) m = let
        m' = declareLocal label m
        in decLabels labels m'
varsSeq True (Seq ((DecAssign label x):ss)) m = do
    varsX x m
    varsSeq (Seq ss) m
varsSeq tl (Seq ((DecAssign label x):ss)) m = do
    varsX x m
    let m' = if not tl then defineLocal label m else defineGlobal label m
    varsSeq tl (Seq ss) m'

varsSeq tl (Seq ((Assign (Var label) x):ss)) m = 
    if declared label m then do
        varsX x m
        let m' = if not tl then defineLocal label m else defineGlobal label m
        varsSeq tl (Seq ss) m'
    else reject $ "Var `" ++ label ++ "` not declared before assignment."
-- things like `a ! 0 <- x + 1`
varsSeq tl (Seq ((Assign lhs@(Op2 "!" _ _) x3):ss)) m = do
    varsX x3 m
    aux lhs m
    varsSeq tl (Seq ss) m
 where
    aux (Op2 "!" x1 x2) m = do
        varsX x2 m
        aux x1 m
    aux x m = varsX x m
 
-- flabel has already been defined globally
varsSeq True (Seq ((Fn flabel fps fss):ss)) m@(_,g) = do
        -- fparams plust the function name, nothing else in local context
    let l  = Set.fromList fps
        -- set lambda params as local decs and defs
        m' = ((l,l),g)
    -- run the function body with `fps` defined
    varsSeq False fss m'
    -- run the next `ss` without `m'`
    varsSeq True (Seq ss) m
-- local function definition
varsSeq False (Seq ((Fn flabel fps fss):ss)) m@(_,g) = do
        -- fparams plust the function name, nothing else in local context
    let l  = flabel `Set.insert` (Set.fromList fps)
        -- set lambda params as local decs and defs
        m' = ((l,l),g)
    -- run the function body with `fps` defined
    varsSeq False fss m'
    -- run the next `ss` without `m'` but map the function name
    varsSeq False (Seq ss) (defineLocal flabel m)
varsSeq tl (Seq ((If x1 ss1 ss2):ss)) m = do
    varsX x1 m
    varsSeq False ss1 m
    varsSeq False ss2 m
    varsSeq tl (Seq ss) m
varsSeq tl (Seq ((While x ss1):ss)) m = do
    varsX x m
    varsSeq False ss1 m
    varsSeq tl (Seq ss) m
varsSeq tl (Seq ((Case x elems):ss)) m = do
    varsX x m
    aux elems m
    varsSeq tl (Seq ss) m
 where
    aux [] m = accept
    aux ((px,ss):elems) m = do
        let m' = varsP px m
        varsSeq tl ss m'
        -- don't keep `m'` since it has pattern vars in it
        aux elems m
-- skip NOP
varsSeq tl (Seq (NOP:ss)) m = varsSeq tl (Seq ss) m
varsSeq tl (Seq ((Block ss1):ss)) m = do
    varsSeq False ss1 m
    varsSeq tl (Seq ss) m
varsSeq tl (Seq ((Stmt x):ss)) m = do
    varsX x m
    varsSeq tl (Seq ss) m
varsSeq True (Seq ((Return x):ss)) m = 
    reject $ "Illegal `return` on top-level."
varsSeq False (Seq ((Return x):ss)) m = do
    varsX x m
    varsSeq False (Seq ss) m
varsSeq True (Seq ((DType label _ elems):ss)) m = varsSeq True (Seq ss) m
varsSeq False (Seq ((DType label _ elems):ss)) m = 
    reject $ "Data Type Definitions only allowed at top-level: `" 
        ++ label ++ "`."
varsSeq True (Seq ((Op op label):ss)) m = varsSeq True (Seq ss) m
varsSeq False (Seq ((Op op label):ss)) m = 
    reject $ "Op Definitions only allowed at top-level: `" 
        ++ op ++ "`."
varsSeq True (Seq ((TypeAlias _ _):ss)) m = varsSeq True (Seq ss) m
varsSeq False (Seq ((TypeAlias label _):ss)) m = 
    reject $ "Type aliases only allowed at top-level: `" 
        ++ label ++ "`."

varsX :: Expr -> VarState -> Either String Bool
varsX (Var label) m = check label m
varsX (PInt _) _ = accept
varsX (PBool _) _ = accept
varsX (PFloat _) _ = accept
varsX (PChar _) _ = accept
varsX (PString _) _ = accept
varsX (PVoid ) _ = accept
varsX (PArray []) _ = accept
varsX (PArray (x:xs)) m = do
    varsX x m
    varsX (PArray xs) m
varsX (PTuple []) m = accept
varsX (PTuple (x:xs)) m = do
    varsX x m
    varsX (PTuple xs) m
varsX (Ifx x1 x2 x3) m = do
    varsX x1 m
    varsX x2 m
    varsX x3 m

varsX (Lambda ps ss) (_,g) = do
    let l  = Set.fromList ps
        -- set lambda params as local decs and defs
        m' = ((l,l),g)
    varsSeq False ss m'

varsX (Ap x1 x2) m = do
    varsX x1 m
    varsX x2 m

varsX (ApNull x) m = do
    varsX x m

varsX (CaseX x elems) m = do
    varsX x m
    aux elems m
 where
    aux [] m = accept
    aux ((px,x):elems) m = do
        let m' = varsP px m
        varsX x m'
        -- don't keep `m'` since it has pattern vars in it
        aux elems m

varsX (Op1 op x) m | op `Set.member` customOpsSet = do
    check op m
    varsX x m

varsX (Op2 op x1 x2) m | op `Set.member` customOpsSet = do
    check op m
    varsX x1 m
    varsX x2 m

varsX (Op1 _ x1) m  = varsX x1 m
varsX (Op2 _ x1 x2) m = do
    varsX x1 m
    varsX x2 m

--varsX x _ = 
--    reject $ "Definition Checker: Unknown Expr\n" ++ show x

-- returns a new `m` from a pattern
varsP :: PatExpr -> VarState -> VarState
-- skip Constructor vars like `Cons` or `Null`
varsP (PatVar label) m | isConsVar label = m
varsP (PatVar label) m = defineLocal label m
varsP (PatInt _) m = m
varsP (PatFloat _) m = m
varsP (PatString _) m = m
varsP (PatChar _) m = m
varsP (PatBool _) m = m
varsP (PatVoid) m = m
varsP (PatAp x1 x2) m = let
    m'  = varsP x1 m
    m'' = varsP x2 m'
    in m''
varsP (PatOp2 _ x1 x2) m = let
    m'  = varsP x1 m
    m'' = varsP x2 m'
    in m''
varsP (AsPattern label x) m = let
    m'  = varsP x m
    m'' = defineLocal label m'
    in m''
varsP (PatArray (x:xs)) m = varsP (PatArray xs) (varsP x m)
varsP (PatArray []) m = m
varsP (PatTuple (x:xs)) m = varsP (PatTuple xs) (varsP x m)
varsP (PatTuple []) m = m
