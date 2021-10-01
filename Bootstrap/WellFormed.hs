module WellFormed
(
wellFormed
) where

import Parse
import qualified Data.Set as Set
import Interp (preloadedLabels)
import Utilities
import Error


--
-- Utilites
--

type Defs = Set.Set Label

emptyDefs = Set.empty

accept :: Either String Bool
accept = Right True
reject :: String -> Either String Bool
reject msg = Left msg



checkContains :: NodeInfo -> ModuleId -> Defs -> [Label] -> Either String Bool
checkContains ni mid _ [] = accept
checkContains ni mid m (label:labels) = 
        if label `Set.member` m 
            then genDupErr ni mid label else checkContains ni mid m labels


--
-- Main
--

-- returns `Left errMsg` if the Expr is not well formed, else `Right True`
wellFormed :: Program -> ModuleId -> Either String Bool
wellFormed p mid = do
    -- no duplicate var declarations in a block
    dupDecs p emptyDefs mid
    -- all vars must be declared and defined before use
    topLevel p mid
    -- ensure that we have no `op`, `data`, or `type` in non-top-level
    lowLevel p mid

-- `Left errMsg` if there are duplicate declarations, otherwise `Right True`
dupDecs :: Seq -> Defs -> ModuleId -> Either String Bool
dupDecs (Seq ((Dec ni x):ss)) m mid = do
    let labels = getDecLabels x
    -- no duplicates allowed
    let dups = getDuplicates labels
    if not $ null dups 
        then genDupErr ni mid $ head dups
        else accept
    checkContains ni mid m labels
    let m' = foldr Set.insert m labels
    dupDecs (Seq ss) m' mid
dupDecs (Seq ((DecAssign ni label _):ss)) m mid =
    if label `Set.member` m then
        genDupErr ni mid label
    else dupDecs (Seq ss) (label `Set.insert` m) mid
dupDecs (Seq (dt@(DType ni _ _ _):ss)) m mid = do
    let labels = getDTLabels dt
    let dups = getDuplicates labels
    if not $ null dups 
        then genDupErr ni mid $ head dups
        else accept
    checkContains ni mid m labels
    let m' = foldr Set.insert m labels
    dupDecs (Seq ss) m' mid

dupDecs (Seq ((Block ni ss1):ss)) m mid = do 
    dupDecs ss1 emptyDefs mid
    dupDecs (Seq ss) m mid
dupDecs (Seq ((If ni _ ss1 ss2):ss)) m mid = do 
    dupDecs ss1 emptyDefs mid
    dupDecs ss2 emptyDefs mid
    dupDecs (Seq ss) m mid
dupDecs (Seq ((WhileSugar ni init update _ ss1):ss)) m mid = do 
    dupDecs init emptyDefs mid
    dupDecs ss1 emptyDefs mid
    dupDecs (Seq ss) m mid
dupDecs (Seq ((While ni _ ss1):ss)) m mid = do 
    dupDecs ss1 emptyDefs mid
    dupDecs (Seq ss) m mid
-- ok if the function label is redefined in the function body
dupDecs (Seq ((Fn ni flabel fps fss):ss)) m mid = do
    if flabel `Set.member` m then
        reject $ makeErrMsg ni mid
            $ "Duplicate function definition for `" ++ flabel ++ "`."
        else accept 
    let dups = getDuplicates $ flabel : fps
    if not $ null dups 
        then reject $ makeErrMsg ni mid
            $ "Duplicate function parameter `" ++ head dups 
            ++ "` for function `" ++ flabel ++ "`."
        else accept
    dupDecs fss (Set.fromList fps) mid
    dupDecs (Seq ss) (flabel `Set.insert` m) mid
dupDecs (Seq (_:ss)) m mid = dupDecs (Seq ss) m mid
dupDecs (Seq []) _ mid = accept      
 where
genDupErr ni mid label = 
    reject $ makeErrMsg ni mid
        $ "Duplicate declaration for `" ++ label ++ "`."

-- ensures that the top-level is well-formed
topLevel :: Seq -> ModuleId -> Either String Bool
topLevel (Seq []) mid = accept
topLevel (Seq ((Dec ni _):ss)) mid =
    reject $ makeErrMsg ni mid
        $ "No declarations allowed on the top-level without an assignment."
topLevel (Seq (s:ss)) mid = topLevel (Seq ss) mid

lowLevel :: Program -> ModuleId -> Either String Bool
lowLevel p mid = do 
    checkLowLevel (f mid) p
 where
    f mid (TypeAlias ni _ _) = reject $ makeErrMsg ni mid
        $ "Type aliases not allowed in low-level."
    f mid (DType ni _ _ _) = reject $ makeErrMsg ni mid
        $ "Data type def not allowed in low-level."
    f mid s = accept

-- runs over the low level of the parse tree applies `f` to each statement
checkLowLevel :: (Stmt -> Either String Bool) -> Seq -> Either String Bool
checkLowLevel f (Seq []) = accept
checkLowLevel f (Seq (s:ss)) = do
    checkLowLevelS f s
    checkLowLevel f (Seq ss)

checkLowLevelS :: (Stmt -> Either String Bool) -> Stmt -> Either String Bool
checkLowLevelS f (NOP _) = accept
checkLowLevelS f (Stmt ni x) = accept
checkLowLevelS f (Block ni ss) = aux f ss
checkLowLevelS f (If ni x ss1 ss2) = aux f ss1 >> aux f ss2
checkLowLevelS f (Fn ni flabel fps fss) = aux f fss
checkLowLevelS f (DType ni _ _ _) = accept
checkLowLevelS f (TypeAlias ni _ _) = accept
checkLowLevelS f (WhileSugar ni init update x ss) = do
    aux f init
    aux f update
    aux f ss
checkLowLevelS f (While ni x ss) = aux f ss
checkLowLevelS f (Case ni x elems) = casef f elems
 where
    casef f [] = accept
    casef f ((_,ss):elems) = aux f ss >> casef f elems
checkLowLevelS f (Cond ni elems) = condf f elems
 where
    condf f [] = accept
    condf f ((_,ss):elems) = aux f ss >> condf f elems
checkLowLevelS f (Dec ni _) = accept
checkLowLevelS f (DecAssign ni _ _) = accept
checkLowLevelS f (Assign ni _ _) = accept
checkLowLevelS f (Return ni _) = accept
 where
aux :: (Stmt -> Either String Bool) -> Seq -> Either String Bool
aux f (Seq []) = accept
aux f (Seq (s:ss)) = f s >> aux f (Seq ss)
