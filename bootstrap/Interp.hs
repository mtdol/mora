module Interp where

import Parse
import qualified Data.Array as Array
import qualified Data.Map as Map

data Value = 
      VInt {vgetInt :: Integer} 
    | VChar {vgetChar :: Char} 
    | VFloat {vgetFloat :: Double} 
    | VBool {vgetBool :: Bool}
    | VTuple2 {v2get1 :: Value, v2get2 :: Value}
    | VTuple3 {v3get1 :: Value, v3get2 :: Value, v3get3 :: Value}
    | VArray {vgetArray :: Array.Array Integer Value, vgetArrayLength :: Integer}
    | VPtr Ptr
    --      label        params  args   body
    | VFn (Maybe Label) [Label] [Expr] Seq
    | VVoid
    deriving (Eq, Show, Ord)

interpTo :: (Value -> a) -> Expr -> State -> (a, [IO Value], State)
interpTo get x m =
    let (v,vs,m') = interpX x m in
        (get v,vs,m')

interpToInt x m     = interpTo vgetInt x m
interpToBool x m    = interpTo vgetBool x m
interpToChar x m    = interpTo vgetChar x m
interpToFloat x m   = interpTo vgetFloat x m

type Label      = String
type Context    = Map.Map Label Value
type Ptr        = Integer
type Heap       = (Map.Map Ptr Value, Ptr)
type State      = (Context, Context, Heap)

-- State := (c := context (stack), g := global context (global vars),
--              (h := heap, ptr := next fresh pointer in heap))

emptyContext    :: Context
emptyContext    = Map.empty
emptyHeap       :: Heap
emptyHeap       = (Map.empty, 0)
emptyState      = (emptyContext, emptyContext, emptyHeap)

-- generates a fresh pointer and a new heap from an old heap
genPtr :: Heap -> (Value, Heap)
genPtr (h, ptr) = (VPtr ptr,(h,ptr+1))

-- generates heap space, maps the given value with a fresh ptr, and returns
-- the ptr and modified heap
allocateValue :: Value -> State -> (Value, State)
allocateValue v (c,g,h) = let
    (vptr,h')   = genPtr h
    m'          = mapToHeap vptr v (c,g,h')
    in (vptr, m')

-- allocates an array with the given values and returns a pointer to it
allocateArray :: [Value] -> State -> (Value, State)
allocateArray rs m = let
    n = toInteger $ length rs
    a = Array.array (0,n-1) [(i,v) | (v,i) <- zip rs [0..n-1]]
    (vptr,m') = allocateValue (VArray a n) m
    in (vptr,m')

xor :: Bool -> Bool -> Bool
xor a b = a /= b

-- creates an array of length `n` with the given value as default
makeArray :: Integer -> a -> Array.Array Integer a
makeArray n v 
    | n < 0     = error "Array size must be >= 0"
    | otherwise = Array.array (0, n-1) [(i,v) | i <- [0..n-1]]


-- takes two ptrs to arrays, appends them together and returns a pointer
-- to the merged array
appendArrays :: Value -> Value -> State -> (Value,State)
appendArrays v1@(VPtr _) v2@(VPtr _) m = let
    (VArray a1 n1) = getFromHeap v1 m
    (VArray a2 n2) = getFromHeap v2 m
    a' = Array.array (0,n1+n2-1) 
            ([(i,(a1 Array.! i)) | i <- [0..n1-1]] 
                ++ [(i+n1,(a2 Array.! i)) | i <- [0..n2-1]])
    (vptr,m') = allocateValue (VArray a' (n1+n2)) m
    in (vptr,m')
    

mapToState :: Label -> Value -> State -> State
mapToState id v (c,g,h)
    | id `Map.member` c = 
        let c' = Map.insert id v c in
            (c', g, h)
    | id `Map.member` g =
        let g' = Map.insert id v g in
            (c, g', h)
    | otherwise = error $ "Could not find var: " ++ id

-- forcibly maps the value to the given global context
mapToGlobalContext :: Label -> Value -> State -> State
mapToGlobalContext id v (c,g,h) =
    let g' = Map.insert id v g in
        (c,g',h)

-- forcibly maps the value to the given local context
mapToLocalContext :: Label -> Value -> State -> State
mapToLocalContext id v (c,g,h) =
    let c' = Map.insert id v c in
        (c',g,h)

-- maps a value onto the heap by a pointer
mapToHeap :: Value -> Value -> State -> State
mapToHeap (VPtr ptr) v (c,g,(h,hptr)) =
    let h' = Map.insert ptr v h in
        (c,g,(h',hptr))

-- gets a value from the local and global contexts
getFromState :: Label -> State -> Value
getFromState id (c,g,_)
    | id `Map.member` c = 
        let v = c Map.! id in case v of
            -- if var is declared but not defined, 
            --  check for a global def anyway
            VVoid -> getFromContext id g
            v     -> v
    | id `Map.member` g = 
        let v = g Map.! id in case v of
            VVoid -> error $ "Var not defined: " ++ id
            v     -> v
    | otherwise         = error $ "Var not declared: " ++ id

-- gets the value from the given context
getFromContext :: Label -> Context -> Value
getFromContext id c
    | id `Map.member` c = 
        let v = c Map.! id in case v of
            VVoid -> error $ "Var not defined: " ++ id
            v     -> v
    | otherwise         = error $ "Var not declared: " ++ id

getFromHeap :: Value -> State -> Value
getFromHeap (VPtr ptr) (_,_,(h,_))
    | ptr `Map.member` h = h Map.! ptr
    | otherwise = error "get: Bad ptr."


listToArray :: [a] -> Array.Array Integer a
listToArray es =
    let n   = toInteger $ length es
        es' = zip es [0..n-1]
    in Array.array (0,n-1) [(i,e) | (e,i) <- es']


--             array                        length     
arrayToList :: Array.Array Integer Value -> Integer -> [Value]
arrayToList a n = aux a n 0 where 
aux a n i | i == n = []
aux a n i = a Array.! i : aux a n (i+1)
    

--                           return status  io stream    memory
interp :: Seq -> [String] -> (Value,        [IO Value],  State)
interp ss args = 
    let m = emptyState
        -- now gather function and type definitions from the top-level
        (ss',m') = interpDefs ss m
        -- now exec global variables
        (vs, (c'',g'',h'')) = interpGlobals ss' m'
        -- find the main function
        vfn@(VFn _ _ _ _) = getFromContext "main" g''
        -- convert args into an in-language array
        (vptrs, m''') = getArrays args (c'',g'',h'')
        (args',am)    = allocateArray vptrs m'''
        -- map the args vptr to local context as `args`
        am'           = mapToLocalContext "args" args' am
    in case vfn of
        -- we must either have `fn main args` or `fn main`
        (VFn _ [] [] fb) -> 
            -- `ret` is the return status of the main function
            let (ret,vs',am'') = interpRunFn fb [] [] am'
            in (ret,vs++vs',am'')
        (VFn _ fps@["args"] [] fb) -> 
            let (ret,vs',am'') = interpRunFn fb fps [(Var "args")] am'
            in (ret,vs++vs',am'')
        _ -> error "Could not find appropriate main function definition."
    where
    getArrays ss m = foldr (\s (vptrs,m) -> 
                     let (vptr,m') = allocateArray (map VChar s) m in (vptr:vptrs,m'))
                   ([],m) ss

-- doesn't look for a main function
interpInteractive :: Seq -> ([IO Value], State)
interpInteractive ss =
    let (ss',m') = interpDefs ss emptyState
        (vs, m'') = interpGlobals ss' m'
    in (vs, m'')

-- grabs all function, type, and op definitions from the top level
--  maps them into the global context and removes them from the program
interpDefs :: Seq -> State -> (Program, State)
interpDefs (Seq []) m   = (Seq [], m)
interpDefs (Seq (s:ss)) m = case s of
    Fn fl fps fb -> 
        let vf = VFn (Just fl) fps [] fb
            m' = mapToGlobalContext fl vf m
        in interpDefs (Seq ss) m'
    Op op ps b -> 
        let vf = VFn (Just op) ps [] b
            m' = mapToGlobalContext op vf m
        in interpDefs (Seq ss) m'
    _ -> aux s $ interpDefs (Seq ss) m 
 where
    aux s (Seq ss,m) = (Seq (s:ss), m)
        

interpGlobals :: Seq -> State -> ([IO Value], State)
interpGlobals (Seq []) m = ([], m)
interpGlobals (Seq ((Dec x):ss)) (c,g,h) =
    let g' = aux x g 
        (vs,m'') = interpGlobals (Seq ss) (c,g',h)
    in (vs,m'') where 
    aux x g = 
        case x of
            Op2 "," (Var id) x2 -> aux x2 (Map.insert id VVoid g)
            Var id              -> Map.insert id VVoid g
interpGlobals (Seq (s:ss)) m = case s of
    Fn _ _ _ -> interpGlobals (Seq ss) m
    _ -> let (_,vs,m')   = interpS s m
             (vs',m'') = interpGlobals (Seq ss) m'
         in (vs++vs',m'')
        
--                           return value    io stream      memory
interpSeq :: Seq -> State -> (Maybe Value,   [IO Value],    State)
interpSeq (Seq []) m       = (Nothing,[],m)
interpSeq (Seq ((Return x):ss)) m = 
    let (v,vs,m') = interpX x m
    in (Just v, vs, m')
interpSeq (Seq (s:ss)) m   = 
    case interpS s m of
        -- no return value, so keep executing
        (Nothing, vs, m') ->
            let (v,vs',m'') = interpSeq (Seq ss) m'
            in (v,vs++vs',m'')
        -- return value, so stop executing
        r@(Just v, _, _) -> r

interpS :: Stmt -> State -> (Maybe Value, [IO Value], State)
interpS (Stmt x) m = 
    let (_,vs,m') = interpX x m in
        (Nothing,vs,m')
interpS (Block ss) m = interpSeq ss m
interpS (NOP) m    = (Nothing,[],m)
-- maps each var into the context with a meaningless `VVoid` value
interpS (Dec x) (c,g,h) = 
    let c' = aux x c in (Nothing,[],(c',g,h)) where 
    aux x c = 
        case x of
            Op2 "," (Var id) x2 -> aux x2 (Map.insert id VVoid c)
            Var id              -> Map.insert id VVoid c

interpS (Assign (Var id) x) m =
    let (v,vs,m') = interpX x m
        m'' = mapToState id v m'
        in
            (Nothing,vs, m'')

interpS (Assign (Op2 "!" lx1 lx2) rx) m =
    let (v,vs,m')     = interpX rx m
        (i,vs',m'')   = interpToInt lx2 m'
        (vptr@(VPtr _),vs'',m''') = interpX lx1 m''
    in
        case getFromHeap vptr m''' of
            VArray a n -> 
                let a'      = VArray (a Array.// [(i,v)]) n
                    m''''   = mapToHeap vptr a' m'''
                in (Nothing,vs++vs'++vs'',m'''')
            _ -> error "Tried to assign to non-array"
       
-- local function definition
interpS (Fn fl fps fb) m = 
    let m' = mapToLocalContext fl (VFn (Just fl) fps [] fb) m
    in (Nothing,[],m')
interpS (Op _ _ _) m = error "Op definitions only allowed in top-level."
interpS (If x ss1 ss2) m = 
    let (b,vs,m'@(c',_,_)) = interpToBool x m
    in
        if b then 
            let (v,vs',(c'',g'',h'')) = interpSeq ss1 m'
            in (v,vs++vs',(c'',g'',h''))
        else 
            let (v,vs',(c'',g'',h'')) = interpSeq ss2 m'
            in (v,vs++vs',(c'',g'',h''))
interpS (While x ss) m = 
    let (b,vs,m'@(c',_,_)) = interpToBool x m 
    in
        if not b then
            (Nothing,vs,m')
        else
            case interpSeq ss m' of
                (Just v,vs',(c'',g'',h'')) -> (Just v,vs++vs',(c'',g'',h''))
                (Nothing,vs',(c'',g'',h'')) ->
                    let (v,vs'',m''') = interpS (While x ss) (c'',g'',h'') in
                    (v,vs++vs'++vs'',m''')

interpOp2 :: 
       (Expr -> State -> (a, [IO Value], State)) 
    -> (Expr -> State -> (b, [IO Value], State))
    -> (a -> b -> c)
    -> (c -> Value)
    -> Expr -> Expr -> State
    -> (Value, [IO Value], State)
interpOp2 f1 f2 op t x1 x2 m = 
    let (v1,vs,m')     = f1 x1 m
        (v2,vs',m'')   = f2 x2 m'
        in
            (t (v1 `op` v2),vs++vs',m'')

interpOp2Generic :: 
       (Value -> Value -> a)
    -> (a -> Value)
    -> Expr -> Expr -> State
    -> (Value, [IO Value], State)
interpOp2Generic op t x1 x2 m = 
    let f             = interpX
        (v1,vs,m')    = f x1 m
        (v2,vs',m'')  = f x2 m'
        in
            (t (v1 `op` v2),vs++vs',m'')

interpOp1 :: 
       (Expr -> State -> (a,[IO Value],State)) 
    -> (a -> b)
    -> (b -> Value)
    -> Expr -> State
    -> (Value, [IO Value], State)
interpOp1 f op t x m = 
    let (v,vs,m')    = f x m in
        (t (op v),vs,m')

--                              results  io stream    memory
interpXs :: [Expr] -> State -> ([Value], [IO Value],  State)
interpXs xs m =
    foldr (\x (rs,vs,m) -> 
                let (r,vs',m') = interpX x m 
                in (r:rs,vs++vs',m')) 
          ([],[],m) xs

-- loads args into fresh local context and runs the expression
-- Note: the fargs `fas` should be in reverse order relative to the fparams `fps`
interpRunFn :: Seq -> [Label] -> [Expr] -> State -> (Value,[IO Value],State)
interpRunFn fb fps fas m =
        -- exec the args
    let (fas',vs,(c',g',h')) = interpXs fas m
                                  --   foldr (\x (afas,avs,am) -> 
                                  --       let (v,vs,m) = interpX x am 
                                  --       in (v:afas, avs++vs, m)) 
                                  -- ([],[],m) fas
        -- load args into fresh local context
        m'   = foldr 
                   (\(p,a) acc -> mapToLocalContext p a acc) 
                   (emptyContext,g',h')
                   (zip fps (reverse fas'))
        -- execute the funtion body
        (v, vs', (_,g'',h'')) = interpSeq fb m'
        -- keep the old local context, take the new global context and heap
        m'' = (c',g'',h'')
    in case v of
        -- no explicit return value, we asume void
        Nothing -> (VVoid, vs++vs', m'')
        -- the return value, pass it on as a value
        Just v  -> (v, vs++vs', m'')

-- The value is the value we are working with.
-- The value stream is all of the values that have been accumulated from
--  function calls.
--
--                          value   io stream    memory
interpX :: Expr -> State -> (Value, [IO Value],  State)
interpX (PBool b) m     = (VBool $ b,[],m)
interpX (PInt i) m      = (VInt $ i,[],m)
interpX (PChar c) m     = (VChar $ c,[],m)
interpX (PFloat f) m    = (VFloat $ f,[],m)
-- disregard type annotations
interpX (Op2 "::" x _) m = interpX x m
interpX (Var id) m = (getFromState id m,[],m)

interpX (PArray xs) m = let
    (rs,vs,m') = interpXs xs m
    (vptr,m'') = allocateArray rs m'
    in (vptr,vs,m'')

interpX (PString s) m = let
    rs = map VChar s
    (vptr,m') = allocateArray rs m
    in (vptr,[],m')

interpX (PTuple xs) m = let
    (vs,ios,m') = interpXs xs m
    v = case vs of
        [v1,v2] -> 
            VTuple2 v1 v2
        [v1,v2,v3] -> 
            VTuple3 v1 v2 v3
        _ -> error "Cannot construct more than 3-tuple."
    in (v,ios,m')

interpX (Lambda lps lb) m = (VFn Nothing lps [] lb,[],m)

interpX (Op2 "+" x1 x2) m =
    interpOp2 interpToInt interpToInt (+) VInt x1 x2 m
interpX (Op2 "+." x1 x2) m =
    interpOp2 interpToFloat interpToFloat (+) VFloat x1 x2 m
interpX (Op2 "-" x1 x2) m =
    interpOp2 interpToInt interpToInt (-) VInt x1 x2 m
interpX (Op2 "-." x1 x2) m =
    interpOp2 interpToFloat interpToFloat (-) VFloat x1 x2 m
interpX (Op2 "*" x1 x2) m =
    interpOp2 interpToInt interpToInt (*) VInt x1 x2 m
interpX (Op2 "*." x1 x2) m =
    interpOp2 interpToFloat interpToFloat (*) VFloat x1 x2 m
interpX (Op2 "/" x1 x2) m =
    let (v1,vs,m')    = interpToInt x1 m
        (v2,vs',m'')  = interpToInt x2 m'
    in (VInt $ floor $ (fromIntegral v1) / (fromIntegral v2),vs++vs',m'')
interpX (Op2 "/." x1 x2) m =
    interpOp2 interpToFloat interpToFloat (/) VFloat x1 x2 m
interpX (Op2 "^" x1 x2) m =
    interpOp2 interpToInt interpToInt (^) VInt x1 x2 m
interpX (Op2 "^." x1 x2) m =
    interpOp2 interpToFloat interpToFloat (**) VFloat x1 x2 m

interpX (Op2 "%" x1 x2) m =
    interpOp2 interpToInt interpToInt (mod) VInt x1 x2 m

interpX (Op2 "=" x1 x2) m =
    interpOp2Generic (==) VBool x1 x2 m
interpX (Op2 "/=" x1 x2) m =
    interpOp2Generic (/=) VBool x1 x2 m

interpX (Op2 ">" x1 x2) m =
    interpOp2Generic (>) VBool x1 x2 m
interpX (Op2 "<" x1 x2) m =
    interpOp2Generic (<) VBool x1 x2 m
interpX (Op2 ">=" x1 x2) m =
    interpOp2Generic (>=) VBool x1 x2 m
interpX (Op2 "<=" x1 x2) m =
    interpOp2Generic (<=) VBool x1 x2 m

interpX (Op2 "and" x1 x2) m =
    interpOp2 interpToBool interpToBool (&&) VBool x1 x2 m
interpX (Op2 "or" x1 x2) m =
    interpOp2 interpToBool interpToBool (||) VBool x1 x2 m
interpX (Op2 "xor" x1 x2) m =
    interpOp2 interpToBool interpToBool (\x y -> x `xor` y) VBool x1 x2 m

-- interpX (Op2 "," x1 x2) m =
--     let (v1,vs,m')    = interpX x1 m
--         (v2,vs',m'')   = interpX x2 m'
--         in (VTup v1 v2,vs++vs',m'')

interpX (Ap (Var "fst") x) m = let
    (v,ios,m') = interpX x m 
    v' = case v of
        VTuple2 v1 v2 -> v1
        VTuple3 v1 v2 v3 -> v1
    in (v',ios,m')
interpX (Ap (Var "snd") x) m = let
    (v,ios,m') = interpX x m 
    v' = case v of
        VTuple2 v1 v2 -> v2
        VTuple3 v1 v2 v3 -> v2
    in (v',ios,m')

interpX (Ap (Var "error") x) m = let 
    (vptr,vs,m') = interpX x m 
    (VArray a n) = getFromHeap vptr m'
    -- convert VChar array to Char list
    msg          = map vgetChar (arrayToList a n)
    in error msg

interpX (Op1 "-" x) m =
    interpOp1 interpToInt (\x -> 0 - x) VInt x m
interpX (Ap (Var "not") x) m =
    interpOp1 interpToBool not VBool x m
interpX (Ap (Var "printChar") x) m = let
    (v,vs,m') = interpToChar x m
    in (VVoid,vs++[return $ VChar v],m')
-- interpX (Op1 "@" x) m =
--     let (s1,vs,m') = interpX (Op1 "show" x) m 
--         (s2,m'') = allocateArray [VChar '\n'] m'
--         (s',m''') = appendArrays s1 s2 m''
--     in
--         (VVoid,vs++[return s'],m''')
interpX (Ap (Var "show") x) m = 
    let (v,vs,m')  = interpX x m 
        s          = map VChar $ aux v m'
        (vptr,m'') = allocateArray s m'
    in
    (vptr,vs,m'') where
    aux v m = 
        case v of
            VInt i      -> show i
            VFloat f    -> show f
            VChar c     -> show c
            VBool b     -> show b
            VTuple2 v1 v2  -> 
                "(" ++ aux v1 m ++ ", " ++ aux v2 m ++ ")"
            VTuple3 v1 v2 v3 -> 
                "(" ++ aux v1 m ++ ", " ++ aux v2 m ++ ", " ++ aux v3 m ++ ")"
            VArray a n  -> showArray a n 0
            VPtr ptr    -> aux (getFromHeap (VPtr ptr) m) m
            v           -> error $ "Invalid type to show: " ++ show v
    showArray :: Array.Array Integer Value -> Integer -> Integer -> String
    showArray _ 0 0 = "[]"
    showArray a 1 0 = "[" ++ aux (a Array.! 0) m ++ "]"
    showArray a n 0 = "[" ++ aux (a Array.! 0) m ++ showArray a n 1
    showArray a n i | n-1 == i = ", " ++ aux (a Array.! i) m ++ "]"
    showArray a n i = ", " ++ aux (a Array.! i) m ++ showArray a n (i+1)


interpX (Ap (Var "Array") x) m =
    let (n,vs,m')   = interpToInt x m 
        (c',g',h')  = m'
        (vptr,h'')  = genPtr h'
        a           = VArray (makeArray n (VInt $ toInteger 0)) n
        m''         = mapToHeap vptr a (c',g',h'')
    in
        (vptr,vs,m'')
interpX (Op2 "!" x1 x2) m =
    case interpX x1 m of
        (vptr@(VPtr _),vs,m') ->
            case getFromHeap vptr m' of
                (VArray a _) -> 
                    let (i,vs',m'') = interpToInt x2 m'
                    in (a Array.! i, vs++vs', m'')
                _ -> error "Tried to array-deref a non-array."
        _ -> error "Tried to array-deref a non-array."

interpX (Op2 "." x1 (Var id)) m =
    case interpX x1 m of
        (vptr@(VPtr _),vs,m') -> 
            case getFromHeap vptr m' of
                VArray _ n -> if id == "length" then (VInt n,vs,m') else
                    error $ "Member `" ++ id ++ "` not included in Object."
                _ -> error "TODO: implement generic object references" -- TODO
        _ -> error "Left side of `.` did not lead to a pointer."

interpX (Ifx x1 x2 x3) m =
    let (b1,vs,m') = interpToBool x1 m in
    if b1 then 
        let (v,vs',m'') = interpX x2 m'
        in (v,vs++vs',m'')
    else 
        let (v,vs',m'') = interpX x3 m'
        in (v,vs++vs',m'')

interpX (Ap x1 x2) m =
    let ((VFn fl fps fas fb),vs,m') = interpX x1 m
    in case fps of
        [] -> error "Tried to apply argument onto nullary function."
        _  ->
            if length fas == length fps - 1 then 
                let (v,vs',m'') = interpRunFn fb fps (x2:fas) m'
                 in (v,vs++vs',m'')
            else (VFn fl fps (x2:fas) fb,vs,m')

interpX (ApNull x) m =
    let ((VFn fl [] [] fb),vs,m') = interpX x m
        (v,vs',m'') = interpRunFn fb [] [] m'
    in (v,vs++vs',m'')

interpX (Op2 "$" x1 x2) m = interpX (Ap x1 x2) m

-- user defined ops
interpX (Op2 op x1 x2) m = let
    (_,g,_) = m
    (VFn _ fps _ fb) = getFromContext op g
    (v,ios,m') = interpRunFn fb fps [x2,x1] m
    in (v,ios,m')

interpX (Op1 op x) m = let
    (_,g,_) = m
    (VFn _ fps _ fb) = getFromContext op g
    (v,ios,m') = interpRunFn fb fps [x] m
    in (v,ios,m')
