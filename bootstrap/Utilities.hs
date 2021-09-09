module Utilities where

import qualified Data.Set as Set
import Parse


-- gets all labels from a `Dec` statement
-- ignore type declarations
getDecLabels (Op2 "::" x _) = getDecLabels x
getDecLabels (Op2 "," x1 x2) = getDecLabels x1 ++ getDecLabels x2
getDecLabels (Var label) = [label]

getDTLabels :: Stmt -> [Label]
getDTLabels (DType _ _ elems) = aux elems
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
