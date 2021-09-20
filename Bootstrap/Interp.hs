module Interp where

import qualified Data.Array as Array
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Char

import Parse
import Error

data Value = 
      VInt {vgetInt :: Integer} 
    | VChar {vgetChar :: Char} 
    | VFloat {vgetFloat :: Double} 
    | VBool {vgetBool :: Bool}
    | VTuple [Value]
    --       the array itself                         the length of the array
    | VArray {vgetArray :: Array.Array Integer Value, vgetArrayLength :: Integer}
    | VPtr Ptr
    -- the `label` is the name of the function (taken from the declaration)
    --  it will be `Nothing` if the VFn came from a lambda
    --
    -- the params are the params
    --
    -- the args are the arguments that the VFn has received so far 
    --  (used to implement currying, the function is called when the 
    --  number of args = the arity of the function)
    --
    -- the body portion is either an in language block of code or a String
    --  representing a preloaded function to execute in the interpreter 
    --
    --    label         mid      params  args           preloaded  body
    | VFn (Maybe Label) ModuleId [Label] [Value] (Either String     Seq)
    -- Value constructor 
    --
    -- Example: `Cons val :: a, next :: List a` would be
    --  `VCons "Cons" 2 []`
    --  with no args applied
    -- 
    --      label  num fields  args
    | VCons Label  Int         [Value]
    -- the `label` is the name of the object, such as `Cons` or `Null`
    --
    -- the `member` tuples of the obj are always listed in the
    --  same order as in the data type declaration
    --
    -- for `(Cons 3 Null)` the VObj would be
    --  `VObj "Cons" [("val", 3), ("next", Null)]`
    --
    --     label  members
    | VObj Label  [Value]
    --        cons name  index of member  args
    | VGetter Label      Int              [Value]
    --        cons name  index of member  args
    | VSetter Label      Int              [Value]
    -- meaningless value, usually the result of an assignment 
    --  or Void returning function
    | VVoid
    deriving (Eq, Show, Ord)

interpTo :: (Value -> a) -> Expr -> ModuleId -> State -> (a, [IO Value], State)
interpTo get x mid m =
    let (v,os,m') = interpX x mid m in
        (get v,os,m')

interpToInt x mid m     = interpTo vgetInt x mid m
interpToBool x mid m    = interpTo vgetBool x mid m
interpToChar x mid m    = interpTo vgetChar x mid m
interpToFloat x mid m   = interpTo vgetFloat x mid m

data GlobalKey = GlobalKey Label ModuleId
    deriving (Show, Eq, Ord)
data GlobalEntry = GlobalValue Value | GlobalRef GlobalKey
    deriving (Show, Eq, Ord)

type Context    = Map.Map Label Value
type GlobalContext = Map.Map GlobalKey GlobalEntry
type Ptr        = Integer
--                 heap itself        the next fresh ptr
type Heap       = (Map.Map Ptr Value, Ptr)
type State      = (Context, GlobalContext, Heap)

-- State := (c := context (stack), g := global context (global vars),
--              (h := heap, ptr := next fresh pointer in heap))

emptyContext    :: Context
emptyContext    = Map.empty
emptyGlobalContext :: GlobalContext
emptyGlobalContext = Map.empty
emptyHeap       :: Heap
emptyHeap       = (Map.empty, 0)
emptyState      = (emptyContext, emptyGlobalContext, emptyHeap)

-- gets all keys from the global context that have the key `mid`
--
-- if `refs` is True, then we only return keys that map toreferences,
--  else we return only keys that map to values
-- 
--
getKeyLabelsFromGlobalContext refs m@(_,g,_) mid = let
    elems = if refs
        then (filter (leadsToRef m) (Map.keys g))
        else (filter (not . leadsToRef m) (Map.keys g))
    in foldr 
        (\(GlobalKey klabel kmid) acc ->
            if kmid == mid then klabel : acc else acc)
        [] elems

leadsToRef :: State -> GlobalKey -> Bool
leadsToRef (_,g,_) key = if key `Map.member` g 
    then case g Map.! key of
        GlobalValue _ -> False
        GlobalRef _ -> True
    else False

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
mapToState :: NodeInfo -> Label -> ModuleId -> Value -> State -> State
mapToState ni label mid v m@(c,g,h)
    | label `Map.member` c = 
        let c' = Map.insert label v c in
            (c', g, h)
    | otherwise = let
        key = GlobalKey label mid
        in if key `Map.member` g then
            mapToGlobalContext label mid v m
            else error $ makeErrMsg ni mid 
                $ "Could not find var: " ++ label

-- returns the key of a reference if a reference exists, else Nothing
getGlobalRef :: GlobalKey -> State -> Maybe GlobalKey
getGlobalRef key m@(_,g,_) = if key `Map.member` g then
    case g Map.! key of
        GlobalRef key' -> Just key'
        _ -> Nothing
    else Nothing

-- forcibly maps the value to the given global context
mapToGlobalContext :: Label -> ModuleId -> Value -> State -> State
mapToGlobalContext label mid v m@(c,g,h) = let
    key = GlobalKey label mid
    in case getGlobalRef key m of
        Just key' -> let g' = Map.insert key' (GlobalValue v) g in
            (c,g',h)
        _ -> let g' = Map.insert key (GlobalValue v) g in
            (c,g',h)

-- forcibly maps the value to the given local context
mapToLocalContext :: Label -> Value -> State -> State
mapToLocalContext label v (c,g,h) =
    let c' = Map.insert label v c in
        (c',g,h)

-- maps a value onto the heap by a pointer
mapToHeap :: Value -> Value -> State -> State
mapToHeap (VPtr ptr) v (c,g,(h,hptr)) =
    let h' = Map.insert ptr v h in
        (c,g,(h',hptr))

-- TODO: don't do VVoid, use Maybe
-- gets a value from the local and global contexts
getFromState :: NodeInfo -> Label -> ModuleId -> State -> Value
getFromState ni label mid m@(c,g,_)
    | label `Map.member` c = 
        let v = c Map.! label in case v of
            VVoid -> error $ makeErrMsg ni mid
                $ "Var not defined: `" ++ label ++ "`."
            v     -> v
    | otherwise = let
        key = GlobalKey label mid
        in if key `Map.member` g then 
            case getFromGlobalContext (GlobalKey label mid) m of
                VVoid -> error $ makeErrMsg ni mid
                    $ "Var not defined: `" ++ label ++ "`."
                v     -> v
            else error $ makeErrMsg ni mid
                $ "Var not declared: `" ++ label ++ "`."

getFromLocalContext :: Label -> State -> Value
getFromLocalContext label (c,_,_) = c Map.! label

getFromGlobalContext :: GlobalKey -> State -> Value
getFromGlobalContext key m@(_,g,_) = case g Map.! key of
    GlobalValue v -> v
    GlobalRef key' -> getFromGlobalContext key' m

showHeap :: State -> String
showHeap (_,_,h) = 
    show h

getFromHeap :: Value -> State -> Value
getFromHeap (VPtr ptr) m@(_,_,(h,_))
    | ptr `Map.member` h = h Map.! ptr
    | otherwise = 
        error $ "get: Bad ptr." ++ "\nheap:\n" ++ showHeap m
            ++ "\n\nptr:\n" ++ show ptr

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

-- returns a new list with the elem at index replaced by `x'`
updateList :: [a] -> a -> Int -> [a]
updateList [] _ _ = []
updateList (_:xs) x' 0 = x' : updateList xs x' (-1)
updateList (x:xs) x' i = x : updateList xs x' (i-1)
    

--                                        return status  io stream    memory
interp :: Program -> ModuleId -> State -> [String] -> (Value,        [IO Value],  State)
interp ss mid m args = 
    let 
        -- now gather function and type definitions from the top-level
        m'  = interpDefs ss mid (mapPreloaded mid m)
        m'' = m'
        -- now exec global variables
        (os, m''') = interpGlobals ss mid m''
        -- find the main function
        vfn = getFromState (NodeInfo 0 0) "main" mid m'''
        -- convert args into an in-language array
        (vptrs, m'''') = getArrays args m'''
        (args',m_)     = allocateArray vptrs m''''
    in case vfn of
        -- we must either have `fn main args` or `fn main`
        (VFn _ _ [] [] (Right fb)) -> 
            -- `ret` is the return status of the main function
            let (ret,os',m_') = runMain vfn Nothing mid m_
            in (ret,os++os',m_')
        (VFn _ _ fps@["args"] [] (Right fb)) -> 
            let (ret,os',m_') = runMain vfn (Just args') mid m_
            in (ret,os++os',m_')
        _ -> error $ makeErrMsgMid mid 
            $ "Could not find appropriate main function definition."
    where
    getArrays ss m = foldr (\s (vptrs,m) -> 
                     let (vptr,m') = allocateArray (map VChar s) m in (vptr:vptrs,m'))
                   ([],m) ss
    -- runs the main function 
    --
    --         `main`   args ptr        
    runMain :: Value -> Maybe Value -> ModuleId -> State 
    --      return value   io stream   memory
        ->  (Value,        [IO Value], State)
    -- no `args`
    runMain vfn@(VFn _ _ [] _ (Right fb)) Nothing mid m = 
        interpRunFn fb [] [] mid m
    -- `args`, gotta get a little dirty
    runMain vfn@(VFn _ _ ["args"] _ (Right fb)) (Just vptr) mid m = let
        m' = mapToLocalContext "args" vptr m
        (v,os,m'') = interpSeq fb mid m'
        in case v of
            Nothing -> (VVoid,os,m'')
            Just v -> (v,os,m'')
            
        

-- doesn't look for a main function
interpInteractive :: Program -> ModuleId -> State -> ([IO Value], State)
interpInteractive ss mid m =
    let m'  = interpDefs ss mid (mapPreloaded mid m)
        m'' = m'
        (os, m''') = interpGlobals ss mid m''
    in (os, m''')


-- makes a preloaded function
makePreloadedFn name arity mid =
    -- ["a3","a2","a1"]
    let as = foldr (\n acc -> ("a"++(show n)) : acc) [] [1..arity] in
        (VFn (Just name) mid as [] (Left name))
    
--                  name   arity  
preloadedLabels :: [(Label,Int)]
preloadedLabels = [
      ("printChar", 1)
    , ("show",      1)
    , ("error",     1)
    , ("Array",     1)
    , ("length",    1)
    , ("ord",       1)
    , ("chr",       1)
  ]

preloadedLabelsSet = Set.fromList $ map fst preloadedLabels

-- loads preloaded functions into a given state
mapPreloaded :: ModuleId -> State -> State
mapPreloaded mid m = let 
    fs = map (\(label,arity) -> makePreloadedFn label arity mid) preloadedLabels
    in foldr (\f@(VFn _ _ _ _ (Left s)) acc -> 
                    mapToGlobalContext s mid f acc)
        m fs

-- grabs all function, type, and op definitions from the top level
--  maps them into the global context
interpDefs :: Program -> ModuleId -> State -> State
interpDefs (Seq []) mid m     = m
interpDefs (Seq (s:ss)) mid m = case s of
    Fn ni fl fps fb -> 
        let vf = VFn (Just fl) mid fps [] (Right fb)
            m' = mapToGlobalContext fl mid vf m
        in interpDefs (Seq ss) mid m'
    DType ni _ _ elems -> let 
        m' = interpDTElems elems mid m
        in interpDefs (Seq ss) mid m'
    _ -> interpDefs (Seq ss) mid m 
 where
interpDTElems :: [DTypeElem] -> ModuleId -> State -> State 
interpDTElems [] _ m = m
interpDTElems ((label,rhs):elems) mid m = let
    --  label   rhs
    -- ("Cons", [((Just "valget", Just "valset"), (Var ... )),
    --  ((Just "nextget", Just "nextset"), (...))])
    --  fields = [(Just "valget", Just "valset"), (Just "nextget", Just "nextset")]
    fields = map fst rhs
    in case fields of 
        -- Argument-less constructors like "Null" can be evaluated immediately
        [] -> let
            obj = (VObj label [])
            (vptr, m') = allocateValue obj m
            m'' = mapToGlobalContext label mid vptr m'
            in interpDTElems elems mid m''
        _ -> let
            m' = makeSettersGetters fields label mid m
            cons = VCons label (length fields) []
            -- map the value constructor to the global context
            m'' = mapToGlobalContext label mid cons m'
            in interpDTElems elems mid m''

-- turns a list of cons fields into getters and setters, and maps
--  them onto the global context
--  
--                      Getter        Setter            cons name
makeSettersGetters :: [(Maybe String, Maybe String)] -> Label     -> ModuleId 
    -> State -> State
makeSettersGetters mbs label mid m = aux mbs label mid 0 m
 where
    aux [] _ _ _ m = m
    -- `i` is the field iterator; when we make a getter/setter
    --  it is important that it knows where in the obj the value
    --  it is indexing is
    aux (mb:mbs) label mid i m = let
        m'  = makeGetter mb label mid i m
        m'' = makeSetter mb label mid i m'
        in aux mbs label mid (i+1) m''
    makeGetter mb label mid i m = case mb of
        (Just getlabel,_) -> let
            vget = (VGetter label i [])
            in mapToGlobalContext getlabel mid vget m
        _ -> m
    makeSetter mb label mid i m = case mb of
        (_,Just setlabel) -> let
            vset = (VSetter label i [])
            in mapToGlobalContext setlabel mid vset m
        _ -> m
            
    
-- ignores toplevel function, op, and type defs and runs all other toplevel
--  statements
--
--                                               io stream    memory
interpGlobals :: Program -> ModuleId -> State -> ([IO Value], State)
interpGlobals (Seq []) mid m = ([], m)
interpGlobals (Seq ((DecAssign ni label x2):ss)) mid m = let
    (v2,os,m') = interpX x2 mid m
    m'' = mapToGlobalContext label mid v2 m'
    (os',m''') = interpGlobals (Seq ss) mid m''
    in (os++os',m''')
interpGlobals (Seq (s:ss)) mid m = case s of
    -- ignore
    Fn _ _ _ _ -> interpGlobals (Seq ss) mid m
    -- ignore
    DType _ _ _ _ -> interpGlobals (Seq ss) mid m
    -- ignore
    TypeAlias _ _ _ -> interpGlobals (Seq ss) mid m
    _ -> let (_,os,m') = interpS s mid m
             (os',m'') = interpGlobals (Seq ss) mid m'
         in (os++os',m'')
        
--                                       return value    io stream      memory
interpSeq :: Seq -> ModuleId -> State -> (Maybe Value,   [IO Value],    State)
interpSeq (Seq []) mid m = (Nothing,[],m)
interpSeq (Seq ((Return ni x):ss)) mid m = 
    let (v,os,m') = interpX x mid m
    in (Just v, os, m')
interpSeq (Seq (s:ss)) mid m = 
    case interpS s mid m of
        -- no return value, so keep executing
        (Nothing, os, m') ->
            let (v,os',m'') = interpSeq (Seq ss) mid m'
            in (v,os++os',m'')
        -- return value, so stop executing
        r@(Just v, _, _) -> r

interpS :: Stmt -> ModuleId -> State -> (Maybe Value, [IO Value], State)
interpS (Stmt ni x) mid m = 
    let (_,os,m') = interpX x mid m in
        (Nothing,os,m')
interpS (Block ni ss) mid m = interpSeq ss mid m
interpS (NOP _) mid m    = (Nothing,[],m)
interpS (DecAssign ni label x2) mid m = let
    (v2,os,m') = interpX x2 mid m
    m'' = mapToLocalContext label v2 m'
    in (Nothing,os,m'')
-- TODO: change VVoid
-- maps each var into the context with a meaningless `VVoid` value
interpS (Dec ni (Var vni label)) mid m = let
    m' = mapToLocalContext label VVoid m
    in (Nothing,[],m')
interpS (Assign ni (Var vni label) x) mid m =
    let (v,os,m') = interpX x mid m
        m'' = mapToState ni label mid v m'
        in (Nothing,os, m'')

-- array indexing
--
--                               ptr  index  rhs
interpS (Assign ni1 (Op2 "!" ni2 lx1  lx2)   rx) mid m =
    let (v,os,m')     = interpX rx mid m
        (i,os',m'')   = interpToInt lx2 mid m'
        (vptr@(VPtr _),os'',m''') = interpX lx1 mid m''
    in
        case getFromHeap vptr m''' of
            VArray a n -> 
                    -- build a new array from the old one
                let a'      = VArray (a Array.// [(i,v)]) n
                    -- map the new array onto the same place in the heap
                    m''''   = mapToHeap vptr a' m'''
                in (Nothing,os++os'++os'',m'''')
            _ -> error "Tried to `!` assign to non-array"
       
-- local function definition
interpS (Fn ni fl fps fb) mid m = 
    error $ makeErrMsg ni mid 
        $ "Local function definitions not allowed."
interpS (If ni x ss1 ss2) mid m = 
    let (b,os,m'@(c',_,_)) = interpToBool x mid m
    in
        if b then 
            let (v,os',(c'',g'',h'')) = interpSeq ss1 mid m'
            in (v,os++os',(c'',g'',h''))
        else 
            let (v,os',(c'',g'',h'')) = interpSeq ss2 mid m'
            in (v,os++os',(c'',g'',h''))
interpS (While ni x ss) mid m = 
    let (b,os,m'@(c',_,_)) = interpToBool x mid m 
    in
        if not b then
            -- guard failed so stop
            (Nothing,os,m')
        else
            case interpSeq ss mid m' of
                -- return value, so stop
                (Just v,os',(c'',g'',h'')) -> (Just v,os++os',(c'',g'',h''))
                -- no return value, keep going
                (Nothing,os',(c'',g'',h'')) ->
                    let (v,os'',m''') = interpS (While ni x ss) mid (c'',g'',h'') in
                    (v,os++os'++os'',m''')

-- case statement
interpS (Case ni x elems) mid m =
    let (v,os,m') = interpX x mid m
        (v',os',m'') = interpCaseStmtElems elems v mid m'
    in (v',os++os',m'')

interpS s mid m = 
    error $ "Interp: Could not match stmt:\n" ++ show s 

interpCaseStmtElems :: [CaseStmtElem] -> Value -> ModuleId -> State 
    -> (Maybe Value,[IO Value],State)
interpCaseStmtElems [] v _ _ = 
    error $ "Pattern matching failed for:\n" 
        ++ show v
interpCaseStmtElems ((px,ss):elems) v mid m = case interpP px v mid m of
    Nothing -> interpCaseStmtElems elems v mid m
    Just m' -> interpSeq ss mid m'

interpOp2 :: 
       (Expr -> ModuleId -> State -> (a, [IO Value], State)) 
    -> (Expr -> ModuleId -> State -> (b, [IO Value], State))
    -> (a -> b -> c)
    -> (c -> Value)
    -> Expr -> Expr -> ModuleId -> State
    -> (Value, [IO Value], State)
interpOp2 f1 f2 op t x1 x2 mid m = 
    let (v1,os,m')     = f1 x1 mid m
        (v2,os',m'')   = f2 x2 mid m'
        in
            (t (v1 `op` v2),os++os',m'')

interpOp2Generic :: 
       (Value -> Value -> a)
    -> (a -> Value)
    -> Expr -> Expr -> ModuleId -> State
    -> (Value, [IO Value], State)
interpOp2Generic op t x1 x2 mid m = 
    let f             = interpX
        (v1,os,m')    = f x1 mid m
        (v2,os',m'')  = f x2 mid m'
        in
            (t (v1 `op` v2),os++os',m'')

interpOp1 :: 
       (Expr -> ModuleId -> State -> (a,[IO Value],State)) 
    -> (a -> b)
    -> (b -> Value)
    -> Expr -> ModuleId -> State
    -> (Value, [IO Value], State)
interpOp1 f op t x mid m = 
    let (v,os,m')    = f x mid m in
        (t (op v),os,m')

-- constructs an Object and returns a pointer to it.
-- The exprs are in normal order.
--                                
interpCons :: Label -> [Value] -> ModuleId -> State -> (Value,[IO Value],State)
interpCons label vs mid m = let
    obj = VObj label vs
    (vptr,m') = allocateValue obj m
    in (vptr,[],m')

--                                          results  io stream    memory
interpXs :: [Expr] -> ModuleId -> State -> ([Value], [IO Value],  State)
interpXs xs mid m =
    foldr (\x (rs,os,m) -> 
                let (r,os',m') = interpX x mid m 
                in (r:rs,os++os',m')) 
          ([],[],m) xs

-- loads args into fresh local context and runs the expression
interpRunFn :: Seq -> [Label] -> [Value] -> ModuleId -> State 
    -> (Value,[IO Value],State)
interpRunFn fb fps fas mid m@(c,g,h) =
        -- prep the args
    let fas' = fas
        -- load args into fresh local context
        m'   = foldr 
                   (\(p,a) acc -> mapToLocalContext p a acc) 
                   (emptyContext,g,h)
                   (zip fps fas')
        -- execute the function body
        (v, os, (_,g',h')) = interpSeq fb mid m'
        -- keep the old local context, take the new global context and heap
        m'' = (c,g',h')
    in case v of
        -- no explicit return value, we asume void
        Nothing -> (VVoid, os, m'')
        -- the return value, pass it on as a value
        Just v  -> (v, os, m'')

-- The value is the value we are working with.
-- The value stream is all of the values that have been accumulated from
--  function calls.
--
--                                      value   io stream    memory
interpX :: Expr -> ModuleId -> State -> (Value, [IO Value],  State)
interpX (PBool _ b) mid m     = (VBool $ b,[],m)
interpX (PInt _ i) mid m      = (VInt $ i,[],m)
interpX (PChar _ c) mid m     = (VChar $ c,[],m)
interpX (PFloat _ f) mid m    = (VFloat $ f,[],m)
interpX (PVoid _) mid m       = (VVoid,[],m)
-- disregard type annotations
interpX (Op2 "::" ni x _) mid m = interpX x mid m
interpX (Var ni label) mid m    = (getFromState ni label mid m,[],m)

interpX (PArray _ xs) mid m = let
    (vs,os,m') = interpXs xs mid m
    (vptr,m'') = allocateArray vs m'
    in (vptr,os,m'')

interpX (PString _ s) mid m = let
    rs = map VChar s
    (vptr,m') = allocateArray rs m
    in (vptr,[],m')

interpX (PTuple _ xs) mid m = let
    (vs,os,m') = interpXs xs mid m 
    v = VTuple vs
    (vptr,m'') = allocateValue v m'
    in (vptr,os,m'')

interpX (Lambda ni lps lb) mid m = (VFn Nothing mid lps [] (Right lb),[],m)

interpX (Op2 "+" ni x1 x2) mid m =
    interpOp2 interpToInt interpToInt (+) VInt x1 x2 mid m
interpX (Op2 "+." ni x1 x2) mid m =
    interpOp2 interpToFloat interpToFloat (+) VFloat x1 x2 mid m
interpX (Op2 "-" ni x1 x2) mid m =
    interpOp2 interpToInt interpToInt (-) VInt x1 x2 mid m
interpX (Op2 "-." ni x1 x2) mid m =
    interpOp2 interpToFloat interpToFloat (-) VFloat x1 x2 mid m
interpX (Op2 "*" ni x1 x2) mid m =
    interpOp2 interpToInt interpToInt (*) VInt x1 x2 mid m
interpX (Op2 "*." ni x1 x2) mid m =
    interpOp2 interpToFloat interpToFloat (*) VFloat x1 x2 mid m
interpX (Op2 "/" ni x1 x2) mid m =
    let (v1,os,m')    = interpToInt x1 mid m
        (v2,os',m'')  = interpToInt x2 mid m'
    in (VInt $ floor $ (fromIntegral v1) / (fromIntegral v2),os++os',m'')
interpX (Op2 "/." ni x1 x2) mid m =
    interpOp2 interpToFloat interpToFloat (/) VFloat x1 x2 mid m
interpX (Op2 "**" ni x1 x2) mid m =
    interpOp2 interpToInt interpToInt (^) VInt x1 x2 mid m
interpX (Op2 "**." ni x1 x2) mid m =
    interpOp2 interpToFloat interpToFloat (**) VFloat x1 x2 mid m

interpX (Op2 "%" ni x1 x2) mid m =
    interpOp2 interpToInt interpToInt (mod) VInt x1 x2 mid m

interpX (Op2 "=" ni x1 x2) mid m =
    interpOp2Generic (==) VBool x1 x2 mid m
interpX (Op2 "/=" ni x1 x2) mid m =
    interpOp2Generic (/=) VBool x1 x2 mid m

interpX (Op2 ">" ni x1 x2) mid m =
    interpOp2Generic (>) VBool x1 x2 mid m
interpX (Op2 "<" ni x1 x2) mid m =
    interpOp2Generic (<) VBool x1 x2 mid m
interpX (Op2 ">=" ni x1 x2) mid m =
    interpOp2Generic (>=) VBool x1 x2 mid m
interpX (Op2 "<=" ni x1 x2) mid m =
    interpOp2Generic (<=) VBool x1 x2 mid m

interpX (Op2 "&&" ni x1 x2) mid m =
    interpOp2 interpToBool interpToBool (&&) VBool x1 x2 mid m
interpX (Op2 "||" ni x1 x2) mid m =
    interpOp2 interpToBool interpToBool (||) VBool x1 x2 mid m

interpX (Op1 "-" ni x) mid m =
    interpOp1 interpToInt (\x -> 0 - x) VInt x mid m

interpX (Op2 "!" ni x1 x2) mid m =
    case interpX x1 mid m of
        (vptr@(VPtr _),os,m') ->
            case getFromHeap vptr m' of
                (VArray a _) -> 
                    let (i,os',m'') = interpToInt x2 mid m'
                    in (a Array.! i, os++os', m'')
                _ -> error "Tried to array-deref a non-array."
        _ -> error "Tried to array-deref a non-array."

interpX (IfX ni x1 x2 x3) mid m =
    let (b1,os,m') = interpToBool x1 mid m in
    if b1 then 
        let (v,os',m'') = interpX x2 mid m'
        in (v,os++os',m'')
    else 
        let (v,os',m'') = interpX x3 mid m'
        in (v,os++os',m'')

interpX (Ap ni x1 x2) mid m = let 
    (v2,os,m') = interpX x2 mid m
    in case interpX x1 mid m' of
    ((VGetter label i _),os',m'') -> let
        -- only one arg for getter, so just run
        (v,os'',m''') = interpVGetter (VGetter label i [v2]) mid m''
        in (v,os++os'++os'',m''')
    ((VSetter label i as),os',m'') -> case as of
        -- currying
        [] -> ((VSetter label i [v2]),os++os',m'')
        -- run case
        [v1] -> let
            (v,os'',m''') = interpVSetter (VSetter label i [v1,v2]) mid m''
            in (v,os++os'++os'',m''')
    ((VCons label n as),os',m'') -> case n of
        0 -> error "Tried to apply argument onto nullary constructor."
        _ -> if length as == n - 1 then
            -- run
            let (vptr,os'',m''') = interpCons label (reverse (v2:as)) mid m''
            in (vptr,os++os'++os'',m''')
            -- currying
            else ((VCons label n (v2:as)),os++os',m'')
    ((VFn fl fmid fps fas fb),os',m'') ->
        case fps of
            [] -> error "Tried to apply argument onto nullary function."
            _  ->
                if length fas == length fps - 1 then 
                    case fb of
                        Right fb ->
                            let (v,os'',m''') = interpRunFn fb fps (reverse (v2:fas)) fmid m''
                            in (v,os++os'++os'',m''')
                        -- preloaded functions
                        Left op -> let
                            (v,os'',m''') = interpPreloadedFn op (reverse (v2:fas)) fmid m''
                            in (v,os++os'++os'',m''')
                -- currying
                else (VFn fl fmid fps (v2:fas) fb,os++os',m'')

interpX (ApNull ni x) mid m =
    let ((VFn fl fmid [] [] fb),os,m') = interpX x mid m
    in case fb of
        Right fb -> let
            (v,os',m'') = interpRunFn fb [] [] fmid m'
            in (v,os++os',m'')
        -- preloaded functions
        Left op -> let
            (v,os',m'') = interpPreloadedFn op [] fmid m'
            in (v,os++os',m'')

interpX (Op2 "$" ni x1 x2) mid m = interpX (Ap ni x1 x2) mid m

interpX (CaseX ni x elems) mid m =
    let (v,os,m') = interpX x mid m
        (v',os',m'') = interpCaseExprElems elems v mid m'
    in (v',os++os',m'')


interpX x mid m = 
    error $ "Interp: Could not match expr:\n" ++ show x


interpCaseExprElems :: [CaseExprElem] -> Value -> ModuleId -> State 
    -> (Value,[IO Value],State)
interpCaseExprElems [] v _ _ =
    error $ "Pattern matching failed for:\n" 
        ++ show v
interpCaseExprElems ((px,x):elems) v mid m = case interpP px v mid m of
    Nothing -> interpCaseExprElems elems v mid m
    Just m' -> interpX x mid m'


-- interprets the preloaded functions
interpPreloadedFn :: String -> [Value] -> ModuleId -> State -> (Value,[IO Value],State)
interpPreloadedFn op vs mid m = case op of
    "printChar" -> let
        [v@(VChar _)] = vs
        in (VVoid,[return $ v],m)
    "show" -> let 
        [v] = vs
        (vptr,m') = showMapValue v m
        in (vptr,[],m')
    "error" -> let
        [v] = vs
        (VArray a n) = getFromHeap v m
        -- convert VChar array to Char list
        msg          = map vgetChar (arrayToList a n)
        in error msg
    "Array" -> let
        [VInt n] = vs
        (c,g,h)  = m
        (vptr,h')  = genPtr h
        a           = VArray (makeArray n (VInt $ toInteger 0)) n
        m''         = mapToHeap vptr a (c,g,h')
        in (vptr,[],m'')
    "length" -> let
        [v] = vs
        (VArray _ n) = getFromHeap v m
        in (VInt n,[],m)
    "ord" -> let
        [VChar c] = vs
        in (VInt $ toInteger $ ord c,[],m)
    "chr" -> let
        [VInt i] = vs
        in (VChar $ chr $ fromIntegral i,[],m)

interpVGetter :: Value -> ModuleId -> State -> (Value,[IO Value],State)
interpVGetter (VGetter label i [v]) mid m = let
    errMsg = "Failed to get `" ++ label ++ "` member."
    in case v of
        vptr@(VPtr _) -> case getFromHeap vptr m of
            (VObj label' mbs) ->
                if label == label' then (mbs !! i,[],m) else
                    error errMsg
            _ -> error errMsg
        _ -> error errMsg

interpVSetter :: Value -> ModuleId -> State -> (Value,[IO Value],State)
interpVSetter (VSetter label i [v1,v2]) mid m = let
    errMsg = "Failed to set `" ++ label ++ "` member."
    in case v1 of
        vptr@(VPtr _) -> case getFromHeap vptr m of
            (VObj label' mbs) ->
                if label == label' then let
                    mbs' = updateList mbs v2 i
                    obj' = (VObj label' mbs')
                    m' = mapToHeap vptr obj' m
                    in (VVoid,[],m')
                else
                    error errMsg
            _ -> error errMsg
        _ -> error errMsg
    

-- shows the value, maps it to the heap, and returns a ptr
showMapValue :: Value -> State -> (Value,State)
showMapValue v m = let
    s = showValue v m
    s' = map VChar $ s
    (vptr,m') = allocateArray s' m
    in (vptr,m')

-- shows the value to a haskell string
showValue :: Value -> State -> String
showValue v m = aux v m where
    aux v m = 
        case v of
            VInt i      -> show i
            VFloat f    -> show f
            VChar c     -> show c
            VBool b     -> show b
            VTuple [v1, v2]  -> 
                "(" ++ aux v1 m ++ ", " ++ aux v2 m ++ ")"
            VTuple [v1,v2,v3] -> 
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


-- interprets a a pattern against a value
interpP :: PatExpr -> Value -> ModuleId -> State -> Maybe State
interpP px v mid m = case px of
    PatInt _ i -> case v of
        VInt i' -> if i==i' then Just m else Nothing
        _ -> Nothing
    PatFloat _ f -> case v of
        VFloat f' -> if f==f' then Just m else Nothing
        _ -> Nothing
    PatChar _ c -> case v of
        VChar c' -> if c==c' then Just m else Nothing
        _ -> Nothing
    PatBool _ b -> case v of
        VBool b' -> if b==b' then Just m else Nothing
        _ -> Nothing
    PatVoid _ -> case v of
        VVoid -> Just m
        _ -> Nothing
    pap@(PatAp _ _ _) -> let
        (label,pxs) = patApToList pap
        in case ptrToObj v m of
            Just (VObj label' mbs) ->
                if label /= label' then Nothing else
                    interpPs (reverse pxs) mbs mid m
            _ -> Nothing
    PatTuple ni pxs -> case ptrToTuple v m of
        Just (VTuple vs) -> interpPs pxs vs mid m
        _ -> Nothing
    PatArray ni pxs -> case ptrToArrayValues v m of
        Just vs -> interpPs pxs vs mid m
        _ -> Nothing
    PatString ni s -> case ptrToArrayValues v m of
        Just vs -> interpPs (map (PatChar ni) s) vs mid m
        _ -> Nothing
    PatVar ni "_" -> Just m
    PatVar ni label -> if isConsVar label then
        case ptrToObj v m of
            Just (VObj label' _) ->
                if label /= label' then Nothing else
                    Just m
            _ -> Nothing
        else Just $ mapToLocalContext label v m
    AsPattern ni label x -> case interpP x v mid m of
        Just m' -> Just $ mapToLocalContext label v m'
        Nothing -> Nothing 
 where
    ptrToObj v m = case v of
        vptr@(VPtr _) -> case getFromHeap vptr m of
            obj@(VObj _ _) -> Just obj
            _ -> Nothing
        _ -> Nothing
    ptrToTuple v m = case v of
        vptr@(VPtr _) -> case getFromHeap vptr m of
            tp@(VTuple _) -> Just tp
            _ -> Nothing
        _ -> Nothing
    ptrToArrayValues v m = case v of
        vptr@(VPtr _) -> case getFromHeap vptr m of
            (VArray a n) -> Just $ arrayToList a n
            _ -> Nothing
        _ -> Nothing

interpPs :: [PatExpr] -> [Value] -> ModuleId -> State -> Maybe State
interpPs [] [] mid m = Just m
interpPs _ [] mid m  = Nothing
interpPs [] _ mid m  = Nothing
interpPs (px:pxs) (v:vs) mid m = case interpP px v mid m of
    Just m' -> interpPs pxs vs mid m'
    Nothing -> Nothing
    

-- converts (PatAp (PatAp (Var "Cons") (PatInt 3)) (Var "Null")) ->
--  ("Cons", [(PatInt 3), (Var "Null")])
patApToList :: PatExpr -> (Label, [PatExpr])
patApToList (PatAp ni (PatVar vni label) px) = (label, [px])
patApToList (PatAp ni px1 px2) = let 
    (label, pxs) = patApToList px1
    in (label, px2:pxs)
patApToList _ = error "pattern: Improper constructor application."
