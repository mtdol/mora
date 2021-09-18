module Main where

import System.Environment
import System.IO
import qualified Data.Array as Array
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List.Split (splitOn)
import qualified System.FilePath as FP
import qualified System.Directory as DIR

import Parse
import Preprocessor
import WellFormed
import Desugar
import Interp

main :: IO ()
main = do 
    args <- getArgs
    handleArgs args

preludeImport :: ModuleStmt
preludeImport = Import "Prelude" (Excluding []) ""
preludeOps :: [ModuleStmt]
preludeOps = [
      OpDec "++"    "append"
    , OpDec ":"     "Cons"
    , OpDec "@"     "printLn" 
    , OpDec "!!"    "lindex"
    ]
includePrelude = True

stdLibLoc :: IO String
stdLibLoc = do
    path <- getExecutablePath
    return $ FP.takeDirectory (FP.takeDirectory path) FP.</> "Lib"


-- turns a path to the main file into a module id
-- ex: "a/b/F.mr" -> "F"
mainLocToModuleId :: String -> ModuleId
mainLocToModuleId loc = FP.dropExtension $ FP.takeFileName loc

maxRecDepth = 400

handleArgs :: [String] -> IO ()
handleArgs ("-e":arg:[]) = do
    loc <- stdLibLoc
    run True (Just arg) emptyState "" loc []
handleArgs (file:args) = 
    run False Nothing emptyState 
        (mainLocToModuleId file) (FP.takeDirectory file) args
handleArgs _ = print "Invalid Args."

--     interactive mode?    file text
run :: Bool ->              Maybe String ->   
--  Memory    module id  project location   args
    State ->  String ->  String ->          [String] 
    -> IO ()
run im text m mid prjloc args = do
    (os,_,_,m) <- aux im text m mid prjloc args 0 Map.empty
    printValues os m
 --   print (getKeyLabelsFromGlobalContext True m "B1")
 where
    aux :: Bool -> Maybe String -> State -> ModuleId -> String -> [String]
        -> Int -> Map.Map ModuleId Manifest
        -> IO ([IO Value], Map.Map ModuleId Manifest, Manifest, State)
    aux _ _ _ _ _ _ ipc _ | ipc >= maxRecDepth =
        error "Maximum import depth exceded. Perhaps there is an import cycle."
--                                     already imported modules
    aux im text m mid prjloc args ipc  aimps = do
        text' <- case text of
            -- if something, then text already supplied, else load from mid
            Just txt -> return txt
            Nothing -> do
                libloc <- stdLibLoc
                path <- moduleIdToPath mid prjloc libloc
                h <- openFile path ReadMode 
                hGetContents h
        -- run the preprocessor
        let (mp,p) = readProg text'
        let (md,imps,ops) = moduleParseToComponents mp
        -- TODO: only import if not already explicitly imported
        let ops' = preludeOps ++ ops
        let imps' = if mid == "Prelude" || not includePrelude
            then imps 
            else preludeImport : imps
        -- well formed check
        let p' = case wellFormed p of {
            Right True -> p;
            Left errMsg -> error errMsg;
            }
        let p'' = desugar p'
        (os,aimps',m') <- runImports mid prjloc imps' m ipc aimps
        if im then
            let (os',m'') = interpInteractive p'' mid m'
            in return (os++os',Map.insert mid md aimps',md,m'')
        -- file mode
        else 
            let (ret,os',m'') = interp p'' mid m' args
            in if ret /= (VInt 0) then 
                    error "Non-zero exit status." 
                else return (os++os',Map.insert mid md aimps',md,m'')

    runImports :: ModuleId -> String -> [ModuleStmt] ->
        State -> Int -> Map.Map ModuleId Manifest
        -> IO ([IO Value], Map.Map ModuleId Manifest, State)
    runImports mid prjloc [] m ipc aimps = return ([],aimps,m)
    runImports mid prjloc (imp:imps) m ipc aimps = do
        (os,aimps',m') <- runImport mid prjloc imp m ipc aimps
        (os',aimps'',m'') <- runImports mid prjloc imps m' ipc aimps'
        return (os++os',aimps'',m'')

    runImport :: ModuleId -> String -> ModuleStmt ->
        State -> Int -> Map.Map ModuleId Manifest
        -> IO ([IO Value], Map.Map ModuleId Manifest, State)
    runImport mid prjloc imp@(Import impMid man asl) m ipc aimps = do
        (os,aimps',md,m') <- doImport impMid prjloc m ipc aimps
        let m'' = mapReferences m' mid impMid md man asl 
        return (os,aimps',m'')

    -- runs the import if not already imported
    doImport :: ModuleId -> String -> State -> Int -> Map.Map ModuleId Manifest
        -> IO ([IO Value], Map.Map ModuleId Manifest, Manifest, State)
    doImport mid prjloc m ipc aimps = if mid `Map.member` aimps 
        then return ([],aimps,aimps Map.! mid,m)
        else aux True Nothing m mid prjloc [] 
            (ipc+1) aimps

    -- every elligible reference is mapped from the target module onto
    -- the current one
    mapReferences :: State -> ModuleId -> ModuleId -> Manifest ->
        Manifest -> Label
        -> State
    mapReferences m currMid trgMid moduleMan impMan asLabel =
        foldr 
            (\elem acc -> 
                -- both manifests must allow the mapping
                if 
                       manifestAllows moduleMan elem 
                    && manifestAllows impMan elem
                    -- the label must not refer to a preloaded function
                    && (not $ elem `Set.member` preloadedLabelsSet)
                then mapReference acc elem currMid trgMid asLabel
                else acc)
            m (getKeyLabelsFromGlobalContext False m trgMid)
    mapReference :: State -> Label -> ModuleId -> ModuleId -> Label
        -> State
    mapReference (c,g,h) elem currMid trgMid asLabel = let
        ref = GlobalRef (GlobalKey elem trgMid)
        elem' = if asLabel == "" then elem else asLabel ++ "." ++ elem
        key = GlobalKey elem' currMid
        g' = Map.insert key ref g
        in (c,g',h)

-- only parses
run' _ (Just text) _ _ _ _ = print $ snd $ readProg text

-- Looks for the path of the given module.
-- First checks the directory where `Main` is (the project directory), 
--  then checks the standard library directory `Lib`.
--
--                module id  path to project   path to Lib  output
moduleIdToPath :: String ->  String ->         String ->    IO String
moduleIdToPath mid prj lib = let
    -- split module id into components, ie `"Data.M"` -> `["Data", "M"]`
    elems = splitOn "." mid
    prjTrg = FP.combine prj (FP.joinPath elems ++ ".mr")
    libTrg = FP.combine lib (FP.joinPath elems ++ ".mr")
    in 
        DIR.doesFileExist prjTrg >>= \a ->
        if a then
            return prjTrg
        else DIR.doesFileExist libTrg >>= \b ->
        if b then
            return libTrg
        else error $ "import: Could not find module: `" ++ mid ++ "`." ++
            "\n\n" ++ prjTrg ++ "\n" ++ libTrg ++ "\n"

-- can the label be exported? ask the manifest.
manifestAllows man label = case man of
    Including ls -> label `elem` ls
    Excluding ls -> not $ label `elem` ls

moduleParseToComponents :: Maybe ModuleData 
    -> (Manifest, [ModuleStmt], [ModuleStmt])
-- if no module declaration is included, we just export everything
moduleParseToComponents (Nothing) = (Excluding [],[],[])
moduleParseToComponents (Just (ModuleData man mss)) = let
    (imps, ops) = aux mss
    in (man,imps,ops)
 where
    aux (op@(OpDec _ _):mss) = let 
        (imps,ops) = aux mss
        in (imps,op:ops)
    aux (imp@(Import _ _ _):mss) = let 
        (imps,ops) = aux mss
        in (imp:imps,ops)
    aux [] = ([],[])

printValues :: [IO Value] -> State -> IO ()
printValues (v:vs) m = do
    v' <- v
    printValue v' m
    printValues vs m
printValues ([]) _ = return ()

printValue :: Value -> State -> IO ()
printValue v m = case v of
    VChar c      -> putChar c
    vptr@(VPtr _) -> printValue (getFromHeap vptr m) m
    VArray a _   -> printArray a m

printArray :: Array.Array Integer Value -> State -> IO ()
printArray a m = let (_,n) = Array.bounds a in aux a m (n+1) 0 where
    aux a m n i | i == n = return ()
    aux a m n i = printValue (a Array.! i) m >> aux a m n (i+1)
