import Parse

desugar :: Seq -> Seq
desugar p =
    scopeRelabel p 0

type It = Int

-- the `count` is just an incrementor to ensure that we generate unique
--   var names. `toplevel` is whether we are currently parsing the toplevel
--
--              toplevel      program  count
scopeRelabel :: Bool ->       Seq ->   It   -> (Seq,It)
scopeRelabel tl (Seq ((Fn _ _ fb):ss)) i = let
    (p,i') <- scopeRelabel False i
    in scopeRelabel tl ss i'
scopeRelabel tl (Seq ((Block b):ss)) i = let
    (p,i') <- scopeRelabel False b i
    in scopeRelabel tl ss i'
scopeRelabel tl (Seq ((If _ ss1 ss2):ss)) i = let
    (p,i') <- scopeRelabel False ss1 i
    (p',i'') <- scopeRelabel False ss1 i'
    in scopeRelabel tl ss i'

scopeRelabel tl (Seq (NOP:ss)) i = scopeRelabel tl ss i
scopeRelabel tl (Seq ((Stmt _):ss)) i = scopeRelabel tl ss i
