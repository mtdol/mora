module WellFormed
(
wellFormed
) where

import Parse
import qualified Data.Set as Set
import Interp (preloadedLabels)
import Utilities


--
-- Utilites
--

type Defs = Set.Set Label

emptyDefs = Set.empty

accept :: Either String Bool
accept = Right True
reject :: String -> Either String Bool
reject msg = Left msg



checkContains :: Defs -> [Label] -> Either String Bool
checkContains _ [] = accept
checkContains m (label:labels) = 
        if label `Set.member` m 
            then genDupErr label else checkContains m labels


--
-- Main
--

-- returns `Left errMsg` if the Expr is not well formed, else `Right True`
wellFormed :: Program -> Either String Bool
wellFormed p = do
    -- no duplicate var declarations in a block
    dupDecs p emptyDefs
    -- all vars must be declared and defined before use
    topLevel p
    -- ensure that we have no `op`, `data`, or `type` in non-top-level
    lowLevel p

-- `Left errMsg` if there are duplicate declarations, otherwise `Right True`
dupDecs :: Seq -> Defs -> Either String Bool
dupDecs (Seq ((Dec x):ss)) m = do
    let labels = getDecLabels x
    -- no duplicates allowed
    let dups = getDuplicates labels
    if not $ null dups 
        then genDupErr $ head dups
        else accept
    checkContains m labels
    let m' = foldr Set.insert m labels
    dupDecs (Seq ss) m'
dupDecs (Seq ((DecAssign label _):ss)) m =
    if label `Set.member` m then
        genDupErr label
    else dupDecs (Seq ss) (label `Set.insert` m)
dupDecs (Seq ((Op op _):ss)) m =
    if op `Set.member` m then
        genDupErr op
    else dupDecs (Seq ss) (op `Set.insert` m)
dupDecs (Seq (dt@(DType _ _ _):ss)) m = do
    let labels = getDTLabels dt
    let dups = getDuplicates labels
    if not $ null dups 
        then genDupErr $ head dups
        else accept
    checkContains m labels
    let m' = foldr Set.insert m labels
    dupDecs (Seq ss) m'

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
    if flabel `Set.member` m then
        reject $ "Duplicate function definition for `" ++ flabel ++ "`."
        else accept 
    let dups = getDuplicates $ flabel : fps
    if not $ null dups 
        then reject $ "Duplicate function parameter `" ++ head dups 
            ++ "` for function `" ++ flabel ++ "`."
        else accept
    dupDecs fss (Set.fromList fps)
    dupDecs (Seq ss) (flabel `Set.insert` m)
dupDecs (Seq (_:ss)) m = dupDecs (Seq ss) m
dupDecs (Seq []) _ = accept      
 where
genDupErr label = 
    reject $ "Duplicate declaration for `" ++ label ++ "`."

-- ensures that the top-level is well-formed
topLevel :: Seq -> Either String Bool
topLevel (Seq []) = accept
topLevel (Seq ((Dec _):ss)) =
    reject $ "No declarations allowed on the top-level without an assignment."
topLevel (Seq (s:ss)) = topLevel (Seq ss)

lowLevel :: Program -> Either String Bool
lowLevel p = do 
    checkLowLevel f p
 where
    f (TypeAlias _ _) = reject $ "Type aliases not allowed in low-level."
    f (DType _ _ _) = reject $ "Data type def not allowed in low-level."
    f (Op _ _) = reject $ "Op alias not allowed in low-level."
    f s = accept

-- runs over the low level of the parse tree applies `f` to each statement
checkLowLevel :: (Stmt -> Either String Bool) -> Seq -> Either String Bool
checkLowLevel f (Seq []) = accept
checkLowLevel f (Seq (s:ss)) = do
    checkLowLevelS f s
    checkLowLevel f (Seq ss)

checkLowLevelS :: (Stmt -> Either String Bool) -> Stmt -> Either String Bool
checkLowLevelS f NOP = accept
checkLowLevelS f (Stmt x) = accept
checkLowLevelS f (Block ss) = aux f ss
checkLowLevelS f (If x ss1 ss2) = aux f ss1 >> aux f ss2
checkLowLevelS f (Fn flabel fps fss) = aux f fss
checkLowLevelS f (Op op label) = accept
checkLowLevelS f (DType _ _ _) = accept
checkLowLevelS f (TypeAlias _ _) = accept
checkLowLevelS f (While x ss) = aux f ss
checkLowLevelS f (Case x elems) = casef f elems
 where
    casef f [] = accept
    casef f ((_,ss):elems) = aux f ss >> casef f elems
checkLowLevelS f (Dec _) = accept
checkLowLevelS f (DecAssign _ _) = accept
checkLowLevelS f (Assign _ _) = accept
checkLowLevelS f (Return _) = accept
 where
aux :: (Stmt -> Either String Bool) -> Seq -> Either String Bool
aux f (Seq []) = accept
aux f (Seq (s:ss)) = f s >> aux f (Seq ss)
