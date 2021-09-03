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
    --       the array itself                         the length of the array
    | VArray {vgetArray :: Array.Array Integer Value, vgetArrayLength :: Integer}
    | VPtr Ptr
    -- the `label` is the name of the function (taken from the declaration)
    --  it will be `Nothing` if the VFn came from a lambda
    --
    -- the params are the params
    --
    -- the args are the arguments that the VFn has received so far 
    --  (used to implement currying, they are exec'd when the num of args = 
    --  the num of params)
    --
    -- the body portion is either an in language block of code or a String
    --  representing a preloaded function to execute in the interpreter 
    --
    --    label         params  args           preloaded  body
    | VFn (Maybe Label) [Label] [Expr] (Either String     Seq)
    -- Value constructor 
    --
    -- Example: `Cons val :: a, next :: List a` would be
    --  `VCons "Cons" ["val", "next"] []`
    --  with no args applied
    -- 
    --      name   fields  args
    | VCons Label [Label] [Expr]
    -- the `label` is the name of the object, such as `Cons` or `Null`
    --
    -- the `member` tuples of the obj are always listed in the
    --  same order as in the data type declaration
    --
    -- for `(Cons 3 Null)` the VObj would be
    --  `VObj "Cons" [("val", 3), ("next", Null)]`
    --
    --     label   member  value 
    | VObj Label [(Label,  Value)]
    -- meaningless value, usually the result of an assignment 
    --  or Void returning function
    | VVoid
    deriving (Eq, Show, Ord)

interpTo :: (Value -> a) -> Expr -> State -> (a, [IO Value], State)
interpTo get x m =
    let (v,os,m') = interpX x m in
        (get v,os,m')

interpToInt x m     = interpTo vgetInt x m
interpToBool x m    = interpTo vgetBool x m
interpToChar x m    = interpTo vgetChar x m
interpToFloat x m   = interpTo vgetFloat x m

type Label      = String
type Context    = Map.Map Label Value
type Ptr        = Integer
--                 heap itself        the next fresh ptr
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

-- generates a pointer for the given object, maps the pointer to the object
--  on the heap, and returns the pointer back
genPtrMap :: Value -> State -> (Value,State)
genPtrMap v m@(c,g,h) = let
    (vptr,h') = genPtr h
    m' = mapToHeap vptr v (c,g,h')
    in (vptr,m')

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
    

-- maps an id to a given state, trying the local context first, and
--  later the global context
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

-- maps a value onto the object by label and returns the modified object
mapToObj :: Value -> Label -> Value -> Value
mapToObj obj@(VObj label mbs) field v = let
    mbs' = aux mbs field v 
    in (VObj label mbs') where
    aux [] _ _ = error $ "Member `" ++ field ++
                        "` not included in `" ++ label ++ "` instance."
    aux ((mb,mbv):mbs) field v | field == mb = (mb,v):mbs
    aux ((mb,mbv):mbs) field v = (mb,mbv) : aux mbs field v

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

-- takes an object and a member, and returns Just Value if the member could be found
getMemberFromObj :: Value -> Label -> Maybe Value
getMemberFromObj (VObj _ mbs) field = aux mbs field where
    aux [] _ = Nothing
    aux ((mb,v):_) field | field == mb = Just v
    aux (_:mbs) field = aux mbs field

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
    

--                               return status  io stream    memory
interp :: Program -> [String] -> (Value,        [IO Value],  State)
interp ss args = 
    let 
        -- now gather function and type definitions from the top-level
        m  = interpDefs ss (mapPreloaded emptyState)
        m' = interpOps ss m
        -- now exec global variables
        (os, (c'',g'',h'')) = interpGlobals ss m'
        -- find the main function
        vfn@(VFn _ _ _ _) = getFromContext "main" g''
        -- convert args into an in-language array
        (vptrs, m''') = getArrays args (c'',g'',h'')
        (args',am)    = allocateArray vptrs m'''
        -- map the args vptr to local context as `args`
        am'           = mapToLocalContext "args" args' am
    in case vfn of
        -- we must either have `fn main args` or `fn main`
        (VFn _ [] [] (Right fb)) -> 
            -- `ret` is the return status of the main function
            let (ret,os',am'') = interpRunFn fb [] [] am'
            in (ret,os++os',am'')
        (VFn _ fps@["args"] [] (Right fb)) -> 
            let (ret,os',am'') = interpRunFn fb fps [(Var "args")] am'
            in (ret,os++os',am'')
        _ -> error "Could not find appropriate main function definition."
    where
    getArrays ss m = foldr (\s (vptrs,m) -> 
                     let (vptr,m') = allocateArray (map VChar s) m in (vptr:vptrs,m'))
                   ([],m) ss

-- doesn't look for a main function
interpInteractive :: Program -> ([IO Value], State)
interpInteractive ss =
    let m'  = interpDefs ss (mapPreloaded emptyState)
        m'' = interpOps  ss m'
        (os, m''') = interpGlobals ss m''
    in (os, m''')


-- makes a preloaded function
makePreloadedFn name arity =
    -- ["a3","a2","a1"]
    let as = foldr (\n acc -> ("a"++(show n)) : acc) [] [1..arity] in
        (VFn (Just name) as [] (Left name))
    

-- loads preloaded functions into a given state
mapPreloaded :: State -> State
mapPreloaded m = let 
    fs = [ 
          makePreloadedFn "printChar"   1
        , makePreloadedFn "show"        1
        , makePreloadedFn "error"       1
        , makePreloadedFn "Array"       1
        , makePreloadedFn "fst"         1
        , makePreloadedFn "snd"         1
        ]
    in foldr (\f@(VFn _ _ _ (Left s)) acc -> mapToGlobalContext s f acc)
        m fs

-- grabs all op names and maps them to their appropriate functions
interpOps :: Program -> State -> State
interpOps (Seq []) m = m
interpOps (Seq (s:ss)) m@(_,g,_) = case s of
    Op opname fname -> case getFromContext fname g of
        vfn@(VFn _ _ _ _) -> let
            m' = mapToGlobalContext opname vfn m
            in interpOps (Seq ss) m'
        cons@(VCons _ _ _) -> let
            m' = mapToGlobalContext opname cons m
            in interpOps (Seq ss) m'
    _ -> interpOps (Seq ss) m 

-- grabs all function, type, and op definitions from the top level
--  maps them into the global context
interpDefs :: Program -> State -> State
interpDefs (Seq []) m   = m
interpDefs (Seq (s:ss)) m = case s of
    Fn fl fps fb -> 
        let vf = VFn (Just fl) fps [] (Right fb)
            m' = mapToGlobalContext fl vf m
        in interpDefs (Seq ss) m'
    DType _ _ elems -> let 
        m' = interpDTElems elems m
        in interpDefs (Seq ss) m'
    _ -> interpDefs (Seq ss) m 
 where
interpDTElems :: [DTypeElem] -> State -> State 
interpDTElems [] m = m
interpDTElems ((label,rhs):elems) m = let
    --  label   rhs
    -- ("Cons", [("val", (Var ... )), ("next", (...))])
    --  fields = ["val", "next"]
    fields = map fst rhs
    in case fields of 
        -- Argument-less constructors like "Null" can be evaluated immediately
        [] -> let
            obj = (VObj label [])
            (vptr, m') = genPtrMap obj m
            m'' = mapToGlobalContext label vptr m'
            in interpDTElems elems m''
        _ -> let
            cons = VCons label fields []
            -- map the value constructor to the global context
            m' = mapToGlobalContext label cons m
            in interpDTElems elems m'
            
    
-- ignores toplevel function, op, and type defs and runs all other toplevel
--  statements
--
--                                   io stream    memory
interpGlobals :: Program -> State -> ([IO Value], State)
interpGlobals (Seq []) m = ([], m)
interpGlobals (Seq ((Dec x):ss)) (c,g,h) =
    -- declare in global context
    let g' = aux x g 
        (os,m'') = interpGlobals (Seq ss) (c,g',h)
    in (os,m'') where 
    aux x g = 
        -- iterate over sub-exprs of `dec` expr
        case x of
            Op2 "," (Var id) x2 -> aux x2 (Map.insert id VVoid g)
            -- TODO: same as `dec` situation below
            Var id              -> Map.insert id VVoid g
interpGlobals (Seq (s:ss)) m = case s of
    -- ignore
    Fn _ _ _ -> interpGlobals (Seq ss) m
    -- ignore
    Op _ _   -> interpGlobals (Seq ss) m
    -- ignore
    DType _ _ _ -> interpGlobals (Seq ss) m
    -- ignore
    TypeAlias _ _ -> interpGlobals (Seq ss) m
    _ -> let (_,os,m') = interpS s m
             (os',m'') = interpGlobals (Seq ss) m'
         in (os++os',m'')
        
--                           return value    io stream      memory
interpSeq :: Seq -> State -> (Maybe Value,   [IO Value],    State)
interpSeq (Seq []) m       = (Nothing,[],m)
interpSeq (Seq ((Return x):ss)) m = 
    let (v,os,m') = interpX x m
    in (Just v, os, m')
interpSeq (Seq (s:ss)) m   = 
    case interpS s m of
        -- no return value, so keep executing
        (Nothing, os, m') ->
            let (v,os',m'') = interpSeq (Seq ss) m'
            in (v,os++os',m'')
        -- return value, so stop executing
        r@(Just v, _, _) -> r

interpS :: Stmt -> State -> (Maybe Value, [IO Value], State)
interpS (Stmt x) m = 
    let (_,os,m') = interpX x m in
        (Nothing,os,m')
interpS (Block ss) m = interpSeq ss m
interpS (NOP) m    = (Nothing,[],m)
-- maps each var into the context with a meaningless `VVoid` value
interpS (Dec x) (c,g,h) = 
    let c' = aux x c in (Nothing,[],(c',g,h)) where 
    aux x c = 
        case x of
            Op2 "," (Var id) x2 -> aux x2 (Map.insert id VVoid c)
            -- TODO: inserting Void might not be the best solution,
            --  maybe try `Maybe` instead
            Var id              -> Map.insert id VVoid c

interpS (Assign (Var id) x) m =
    let (v,os,m') = interpX x m
        m'' = mapToState id v m'
        in
            (Nothing,os, m'')

-- array indexing
--
--                       ptr  index  rhs
interpS (Assign (Op2 "!" lx1  lx2)   rx) m =
    let (v,os,m')     = interpX rx m
        (i,os',m'')   = interpToInt lx2 m'
        (vptr@(VPtr _),os'',m''') = interpX lx1 m''
    in
        case getFromHeap vptr m''' of
            VArray a n -> 
                    -- build a new array from the old one
                let a'      = VArray (a Array.// [(i,v)]) n
                    -- map the new array onto the same place in the heap
                    m''''   = mapToHeap vptr a' m'''
                in (Nothing,os++os'++os'',m'''')
            _ -> error "Tried to `!` assign to non-array"

-- obj indexing
--
--                       ptr  label         rhs
interpS (Assign (Op2 "." lx1  (Var label))  rx) m =
    let (v,os,m') = interpX rx m
        (vptr@(VPtr _),os',m'') = interpX lx1 m'
    in case getFromHeap vptr m'' of
        obj@(VObj _ _) -> let
            obj' = mapToObj obj label v
            m''' = mapToHeap vptr obj' m''
            in (Nothing,os++os',m''')
        _ -> error "Tried to `.` assign onto non-object."
       
-- local function definition
interpS (Fn fl fps fb) m = 
    let m' = mapToLocalContext fl (VFn (Just fl) fps [] (Right fb)) m
    in (Nothing,[],m')
interpS (Op _ _) m = error "Op definitions only allowed in top-level."
interpS (If x ss1 ss2) m = 
    let (b,os,m'@(c',_,_)) = interpToBool x m
    in
        if b then 
            let (v,os',(c'',g'',h'')) = interpSeq ss1 m'
            in (v,os++os',(c'',g'',h''))
        else 
            let (v,os',(c'',g'',h'')) = interpSeq ss2 m'
            in (v,os++os',(c'',g'',h''))
interpS (While x ss) m = 
    let (b,os,m'@(c',_,_)) = interpToBool x m 
    in
        if not b then
            -- guard failed so stop
            (Nothing,os,m')
        else
            case interpSeq ss m' of
                -- return value, so stop
                (Just v,os',(c'',g'',h'')) -> (Just v,os++os',(c'',g'',h''))
                -- no return value, keep going
                (Nothing,os',(c'',g'',h'')) ->
                    let (v,os'',m''') = interpS (While x ss) (c'',g'',h'') in
                    (v,os++os'++os'',m''')

interpOp2 :: 
       (Expr -> State -> (a, [IO Value], State)) 
    -> (Expr -> State -> (b, [IO Value], State))
    -> (a -> b -> c)
    -> (c -> Value)
    -> Expr -> Expr -> State
    -> (Value, [IO Value], State)
interpOp2 f1 f2 op t x1 x2 m = 
    let (v1,os,m')     = f1 x1 m
        (v2,os',m'')   = f2 x2 m'
        in
            (t (v1 `op` v2),os++os',m'')

interpOp2Generic :: 
       (Value -> Value -> a)
    -> (a -> Value)
    -> Expr -> Expr -> State
    -> (Value, [IO Value], State)
interpOp2Generic op t x1 x2 m = 
    let f             = interpX
        (v1,os,m')    = f x1 m
        (v2,os',m'')  = f x2 m'
        in
            (t (v1 `op` v2),os++os',m'')

interpOp1 :: 
       (Expr -> State -> (a,[IO Value],State)) 
    -> (a -> b)
    -> (b -> Value)
    -> Expr -> State
    -> (Value, [IO Value], State)
interpOp1 f op t x m = 
    let (v,os,m')    = f x m in
        (t (op v),os,m')

-- constructs an Object and returns a pointer to it.
-- The exprs are in normal order.
--                                
interpCons :: Label -> [Label] -> [Expr] -> State -> (Value,[IO Value],State)
interpCons label fields xs m = let
    (vs,os,m') = interpXs (reverse xs) m
    mbs = zip (reverse fields) vs
    obj = VObj label (reverse mbs)
    (vptr,m'') = genPtrMap obj m'
    in (vptr,os,m'')

--                              results  io stream    memory
interpXs :: [Expr] -> State -> ([Value], [IO Value],  State)
interpXs xs m =
    foldr (\x (rs,os,m) -> 
                let (r,os',m') = interpX x m 
                in (r:rs,os++os',m')) 
          ([],[],m) xs

-- loads args into fresh local context and runs the expression
interpRunFn :: Seq -> [Label] -> [Expr] -> State -> (Value,[IO Value],State)
interpRunFn fb fps fas m =
        -- exec the args
    let (fas',os,(c',g',h')) = interpXs (reverse fas) m
        -- load args into fresh local context
        m'   = foldr 
                   (\(p,a) acc -> mapToLocalContext p a acc) 
                   (emptyContext,g',h')
                   (zip (reverse fps) fas')
        -- execute the funtion body
        (v, os', (_,g'',h'')) = interpSeq fb m'
        -- keep the old local context, take the new global context and heap
        m'' = (c',g'',h'')
    in case v of
        -- no explicit return value, we asume void
        Nothing -> (VVoid, os++os', m'')
        -- the return value, pass it on as a value
        Just v  -> (v, os++os', m'')

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
    (vs,os,m') = interpXs xs m
    (vptr,m'') = allocateArray vs m'
    in (vptr,os,m'')

interpX (PString s) m = let
    rs = map VChar s
    (vptr,m') = allocateArray rs m
    in (vptr,[],m')

interpX (PTuple xs) m = let
    (vs,os,m') = interpXs xs m
    v = case vs of
        [v1,v2] -> 
            VTuple2 v1 v2
        [v1,v2,v3] -> 
            VTuple3 v1 v2 v3
        _ -> error "Cannot construct more than 3-tuple."
    in (v,os,m')

interpX (Lambda lps lb) m = (VFn Nothing lps [] (Right lb),[],m)

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
    let (v1,os,m')    = interpToInt x1 m
        (v2,os',m'')  = interpToInt x2 m'
    in (VInt $ floor $ (fromIntegral v1) / (fromIntegral v2),os++os',m'')
interpX (Op2 "/." x1 x2) m =
    interpOp2 interpToFloat interpToFloat (/) VFloat x1 x2 m
interpX (Op2 "**" x1 x2) m =
    interpOp2 interpToInt interpToInt (^) VInt x1 x2 m
interpX (Op2 "**." x1 x2) m =
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

-- interpX (Op2 "and" x1 x2) m =
--     interpOp2 interpToBool interpToBool (&&) VBool x1 x2 m
-- interpX (Op2 "or" x1 x2) m =
--     interpOp2 interpToBool interpToBool (||) VBool x1 x2 m
-- interpX (Op2 "xor" x1 x2) m =
--     interpOp2 interpToBool interpToBool (\x y -> x `xor` y) VBool x1 x2 m

interpX (Op1 "-" x) m =
    interpOp1 interpToInt (\x -> 0 - x) VInt x m

interpX (Op2 "!" x1 x2) m =
    case interpX x1 m of
        (vptr@(VPtr _),os,m') ->
            case getFromHeap vptr m' of
                (VArray a _) -> 
                    let (i,os',m'') = interpToInt x2 m'
                    in (a Array.! i, os++os', m'')
                _ -> error "Tried to array-deref a non-array."
        _ -> error "Tried to array-deref a non-array."

interpX (Op2 "." x1 (Var id)) m =
    case interpX x1 m of
        (vptr@(VPtr _),os,m') -> 
            case getFromHeap vptr m' of
                VArray _ n -> if id == "length" then (VInt n,os,m') else
                    error $ "Member `" ++ id ++ "` not included in Object."
                obj@(VObj label _) -> case getMemberFromObj obj id of
                    Nothing -> error $ "Member `" ++ id ++
                        "` not included in `" ++ label ++ "` instance."
                    Just v -> (v,os,m')
                _ -> error "`.`: Ptr did not lead to a reference-able object."
        _ -> error "Left side of `.` did not lead to a pointer."

interpX (Ifx x1 x2 x3) m =
    let (b1,os,m') = interpToBool x1 m in
    if b1 then 
        let (v,os',m'') = interpX x2 m'
        in (v,os++os',m'')
    else 
        let (v,os',m'') = interpX x3 m'
        in (v,os++os',m'')

interpX (Ap x1 x2) m = case interpX x1 m of
    ((VCons label fields as),os,m') -> case fields of
        [] -> error "Tried to apply argument onto nullary constructor."
        _ -> if length as == length fields - 1 then
            let (obj,os',m'') = interpCons label fields (reverse (x2:as)) m'
            in (obj,os++os',m'')
        else ((VCons label fields (x2:as)),os,m')
    ((VFn fl fps fas fb),os,m') ->
        case fps of
            [] -> error "Tried to apply argument onto nullary function."
            _  ->
                if length fas == length fps - 1 then 
                    case fb of
                        Right fb ->
                            let (v,os',m'') = interpRunFn fb fps (reverse (x2:fas)) m'
                            in (v,os++os',m'')
                        -- preloaded functions
                        Left op -> let
                            (v,os',m'') = interpPreloadedFn op (reverse (x2:fas)) m'
                            in (v,os++os',m'')
                else (VFn fl fps (x2:fas) fb,os,m')

interpX (ApNull x) m =
    let ((VFn fl [] [] fb),os,m') = interpX x m
    in case fb of
        Right fb -> let
            (v,os',m'') = interpRunFn fb [] [] m'
            in (v,os++os',m'')
        -- preloaded functions
        Left op -> let
            (v,os',m'') = interpPreloadedFn op [] m'
            in (v,os++os',m'')

interpX (Op2 "$" x1 x2) m = interpX (Ap x1 x2) m

-- user defined ops
interpX (Op2 op x1 x2) m = interpX (Ap (Ap (Var op) x1) x2) m
interpX (Op1 op x) m = interpX (Ap (Var op) x) m


-- interprets the preloaded functions
interpPreloadedFn :: String -> [Expr] -> State -> (Value,[IO Value],State)
interpPreloadedFn op xs m = case op of
    "printChar" -> let
        [x] = xs
        (v,os,m') = interpToChar x m
        in (VVoid,os++[return $ VChar v],m')
    "show" -> let 
        [x] = xs
        in interpShow x m
    "error" -> let
        [x] = xs
        (vptr,os,m') = interpX x m 
        (VArray a n) = getFromHeap vptr m'
        -- convert VChar array to Char list
        msg          = map vgetChar (arrayToList a n)
        in error msg
    "Array" -> let
        [x] = xs
        (n,os,m')   = interpToInt x m 
        (c',g',h')  = m'
        (vptr,h'')  = genPtr h'
        a           = VArray (makeArray n (VInt $ toInteger 0)) n
        m''         = mapToHeap vptr a (c',g',h'')
        in (vptr,os,m'')
    "fst" -> let
        [x] = xs
        (v,os,m') = interpX x m 
        v' = case v of
            VTuple2 v1 v2 -> v1
            VTuple3 v1 v2 v3 -> v1
        in (v',os,m')
    "snd" -> let
        [x] = xs
        (v,os,m') = interpX x m 
        v' = case v of
            VTuple2 v1 v2 -> v2
            VTuple3 v1 v2 v3 -> v2
        in (v',os,m')

interpShow :: Expr -> State -> (Value,[IO Value],State)
interpShow x m = 
    let (v,os,m')  = interpX x m 
        s          = map VChar $ aux v m'
        (vptr,m'') = allocateArray s m'
    in
    (vptr,os,m'') where
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
