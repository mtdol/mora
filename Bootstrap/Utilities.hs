module Utilities where

import qualified Data.Set as Set

import Parse
import Error

-- gets all labels from a `Dec` statement
-- ignore type declarations
getDecLabels (Op2 "::" ni x _) = getDecLabels x
getDecLabels (Op2 "," ni x1 x2) = getDecLabels x1 ++ getDecLabels x2
getDecLabels (Var _ label) = [label]

getDTLabels :: Stmt -> [Label]
getDTLabels (DType ni _ _ elems) = aux elems
 where
    aux :: [DTypeElem] -> [Label]
    aux [] = []
    aux ((consLabel,consElems):elems) = 
        (consLabel : aux2 consElems) ++ aux elems
    aux2 :: [((Maybe Label,Maybe Label),Expr)] -> [Label]
    aux2 [] = []
    aux2 (((Nothing,Nothing),_):celems) = aux2 celems
    aux2 (((Just getter,Nothing),_):celems) = getter : aux2 celems
    aux2 (((Nothing,Just setter),_):celems) = setter : aux2 celems
    aux2 (((Just getter,Just setter),_):celems) = getter : setter : aux2 celems

getDuplicates xs = aux xs Set.empty
 where
    aux [] _ = []
    aux (x:xs) m | x `Set.member` m = x : aux xs m
    aux (x:xs) m = aux xs (x `Set.insert` m)


-- gathers all functions at the top-level of the given `Seq` and returns then.
--
-- Also removes the functions from the top-level.
gatherFns :: Seq -> ([Stmt],Seq)
gatherFns (Seq []) = ([],Seq [])
gatherFns (Seq (s@(Fn _ _ _ _):ss)) = let
    (fns,ss') = gatherFns (Seq ss)
    in (s:fns,ss')
gatherFns (Seq (s:ss)) = let
    (fns,(Seq ss')) = gatherFns (Seq ss)
    in (fns,Seq (s:ss'))
