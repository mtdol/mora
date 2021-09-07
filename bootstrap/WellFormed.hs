module WellFormed
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

emptyDefs = Set.empty

accept :: Either String Bool
accept = Right True
reject :: String -> Either String Bool
reject msg = Left msg


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
    topLevel p

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

-- ensures that the top-level is well-formed
topLevel :: Seq -> Either String Bool
topLevel (Seq []) = accept
topLevel (Seq ((Dec _):ss)) =
    reject $ "No declarations allowed on the top-level without an assignment."
topLevel (Seq (s:ss)) = topLevel (Seq ss)
