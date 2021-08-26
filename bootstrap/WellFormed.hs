module WellFormed
(
wellFormed
) where

import Parse
import qualified Data.Set as Set

wellFormed :: Seq -> Bool
wellFormed s = 
    syntax s -- && vars s Set.empty


syntax :: Seq -> Bool
syntax (Seq ss) = 
    and (map syntaxS ss)

syntaxS :: Stmt -> Bool
syntaxS (Stmt x) = syntaxX x
syntaxS (If x ss1 ss2) = 
    syntaxX x && syntax ss1 && syntax ss2
syntaxS (Fn _ _ ss) = syntax ss 
syntaxS (While x ss) = 
    syntaxX x && syntax ss  
syntaxS (Assign x1 x2) = 
    syntaxX x1 && syntaxX x2
syntaxS (Return x) = syntaxX x
syntaxS (NOP) = True
syntaxS (Dec x) = aux x where 
    aux x = case x of
        (Var _)   -> True
        (Op2 "::" x _) -> aux x
        (Op2 "," x1 x2) -> aux x1 && aux x2
        _         -> False

syntaxX :: Expr -> Bool
syntaxX _ = True

{-
vars :: Stmt -> Set.Set String -> Bool
vars (Seq ss) c = 
    and (map (\s -> vars s c) ss)
vars (Stmt x) c = varsX x c
vars (If x s1 s2) c = 
    varsX x c && vars s1 c && vars s2 c
vars (Fn n ps b) c = True -- TODO
vars (While x s) c =
    varsX x c && vars s c
vars (Dec x) c = True -- TODO
vars (Assign x1 x2) c = True -- TODO
vars (Return x) c = varsX x c

varsX :: Expr -> Set.Set String -> Bool
varsX (PInt _) _ = True
varsX (PString _) _ = True
varsX (PBool _) _ = True
varsX (Var s) c = s `Set.member` c
varsX (Op2 "." x1 x2) c = True -- TODO
varsX (Ap x1 x2) c = varsX x1 c && varsX x2 c
varsX (Ifx x1 x2 x3) c = 
    varsX x1 c && varsX x2 c && varsX x3 c
varsX (Op2 _ x1 x2) c = 
    varsX x1 c && varsX x2 c
varsX (Op1 _ x) c = varsX x c
-}
