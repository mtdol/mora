module Interp where

import Parse
import qualified Data.Map as Map

data Value = 
      VInt {vgetInt :: Integer} 
    | VString {vgetString :: String} 
    | VChar {vgetChar :: Char} 
    | VFloat {vgetFloat :: Double} 
    | VBool {vgetBool :: Bool}
    | VTup {vgetFirst :: Value, vgetSecond :: Value}
    | VIO {getIO :: Value}
    | VVoid
    deriving (Eq, Show, Ord)

interpToInt x c      = vgetInt $ interpX x c
interpToString x c   = vgetString $ interpX x c
interpToBool x c     = vgetBool $ interpX x c
interpToChar x c     = vgetChar $ interpX x c
interpToFloat x c    = vgetFloat $ interpX x c

type Label = String
type Context = Map.Map Label Value

emptyContext = Map.empty

xor :: Bool -> Bool -> Bool
xor a b = a /= b

interp :: Seq -> Context -> ([Value], Context)
interp (Seq []) c       = ([VVoid], c)
interp (Seq (s:ss)) c     = 
    let (vs, c')     = interpS s c 
        (vs', c'')   = interp (Seq ss) c' in
            (vs ++ vs', c'')

interpS :: Stmt -> Context -> ([Value], Context)
interpS (Stmt x) c   = ([interpX x c], c)
interpS (NOP) c      = ([VVoid], c)
-- maps each var into the context with a meaningless `VVoid` value
interpS (Dec x) c    = ([VVoid], aux x c) where 
    aux x c = 
        case x of
            Op2 "," (Var id) x2 -> aux x2 (Map.insert id VVoid c)
            Var id              -> Map.insert id VVoid c
interpS (Assign (Var id) x) c 
    | id `Map.member` c = 
        let c' = Map.insert id (interpX x c) c in
            ([VVoid], c')
    | otherwise = error $ "Could not find var: " ++ id
interpS (If x ss1 ss2) c = 
    if interpToBool x c then interp ss1 c else interp ss2 c
interpS (While x ss) c = 
    if not $ interpToBool x c then
        ([VVoid], c) 
    else
        let (vs, c')    = interp ss c 
            (vs', c'')  = interpS (While x ss) c' in
                (vs ++ vs', c'')

interpX :: Expr -> Context -> Value
interpX (PBool b) _     = VBool $ b
interpX (PInt i) _      = VInt $ i
interpX (PString s) _   = VString $ s
interpX (PChar c) _     = VChar $ c
interpX (PFloat f) _    = VFloat $ f
-- disregard type annotations
interpX (Op2 "::" x _) c = interpX x c
interpX (Var id) c 
    | id `Map.member` c = 
        let v = c Map.! id in case v of
            VVoid -> error $ "Var not defined: " ++ id
            v     -> v
    | otherwise         = error $ "Var not declared: " ++ id

interpX (Op2 "+" x1 x2) c =
    VInt $ interpToInt x1 c + interpToInt x2 c
interpX (Op2 "+." x1 x2) c =
    VFloat $ interpToFloat x1 c + interpToFloat x2 c
interpX (Op2 "-" x1 x2) c =
    VInt $ interpToInt x1 c - interpToInt x2 c
interpX (Op2 "-." x1 x2) c =
    VFloat $ interpToFloat x1 c - interpToFloat x2 c
interpX (Op2 "*" x1 x2) c =
    VInt $ interpToInt x1 c * interpToInt x2 c
interpX (Op2 "*." x1 x2) c =
    VFloat $ interpToFloat x1 c * interpToFloat x2 c
interpX (Op2 "/" x1 x2) c =
    VInt $ floor $
        fromIntegral (interpToInt x1 c) 
            / fromIntegral (interpToInt x2 c)
interpX (Op2 "/." x1 x2) c =
    VFloat $ interpToFloat x1 c / interpToFloat x2 c

interpX (Op2 "=" x1 x2) c =
    VBool $ interpX x1 c == interpX x2 c
interpX (Op2 "/=" x1 x2) c =
    VBool $ interpX x1 c /= interpX x2 c

interpX (Op2 ">" x1 x2) c =
    VBool $ interpX x1 c > interpX x2 c
interpX (Op2 "<" x1 x2) c =
    VBool $ interpX x1 c < interpX x2 c
interpX (Op2 ">=" x1 x2) c =
    VBool $ interpX x1 c >= interpX x2 c
interpX (Op2 "<=" x1 x2) c =
    VBool $ interpX x1 c <= interpX x2 c

interpX (Op2 "and" x1 x2) c =
    VBool $ interpToBool x1 c && interpToBool x2 c
interpX (Op2 "or" x1 x2) c =
    VBool $ interpToBool x1 c || interpToBool x2 c
interpX (Op2 "xor" x1 x2) c =
    VBool $ interpToBool x1 c `xor` interpToBool x2 c

interpX (Op2 "," x1 x2) c =
    VTup (interpX x1 c) (interpX x2 c)

interpX (Op1 "fst" x) c =
    vgetFirst (interpX x c)
interpX (Op1 "snd" x) c =
    vgetSecond (interpX x c)

interpX (Op1 "-" x) c =
    VInt $ 0 - interpToInt x c
interpX (Op1 "not" x) c =
    VBool $ not $ interpToBool x c
interpX (Op1 "printChar" x) c =
    VIO $ VChar $ interpToChar x c
-- TODO: `@` is only here for debug purposes
-- eventually we will have this in `std.mr`
interpX (Op1 "@" x) c =
    let (VString v) = interpX (Op1 "show" x) c in
        VIO $ VString $ v ++ "\n"
interpX (Op1 "show" x) c = VString $ aux $ interpX x c where
    aux v = 
        case v of
            VInt i      -> show i
            VFloat f    -> show f
            VChar c     -> show c
            VString s   -> show s
            VBool b     -> show b
            VTup v1 v2  -> "(" ++ aux v1 ++ ", " ++ aux v2 ++ ")"
            v           -> error $ "Invalid type to show: " ++ show v

interpX (Ifx x1 x2 x3) c =
    if interpToBool x1 c then interpX x2 c else interpX x3 c 
