module Preprocessor where

import Text.ParserCombinators.Parsec
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Char as Char
import System.IO
import System.Directory
import System.Environment
import System.FilePath

-- location of the directory that modules are stored
libLoc :: IO String
libLoc = do 
    -- get the path of the current script
    exePath <- getExecutablePath
    -- works as long as the `Main` exe is in directory next to `lib` 
    let path = takeDirectory $ takeDirectory exePath
    let target = joinPath [path, "lib"]
    return $ target

-- returns the location of file relative to the standard module library
getFromLib :: String -> IO String
getFromLib file = do
    lib <- libLoc
    return $ joinPath [lib, file]

-- merges the two strings together and returns them IO wrapped
getFromRel :: String -> String -> IO String
getFromRel s1 s2 = canonicalizePath (takeDirectory s1 </> s2)

trim = List.dropWhileEnd Char.isSpace . List.dropWhile Char.isSpace

parseFail d ln = error $ "line " ++ (show ln) ++ ": " ++
    "Improperly formed preprocessor directive:\n" ++ d

maxRecDepth = 900

-- applies all preprocessor directives and returns back 
--  the modified program text
--
--         input text  recursion counter  curr file  output text
process :: String ->   Int ->             String ->  IO String
process _ r cf | r >= maxRecDepth = 
    error "Maximum include depth exceded. Perhaps there is an include cycle."
process text r cf = do
    let ls  = lines text
    ls' <- sequence $ applyDirs ls 1 r cf
    return $ unlines ls'

getFileText :: String -> IO String
getFileText file = do
    let file' = file
    h <- openFile file' ReadMode
    c <- hGetContents h
    --hClose h
    return c

--           lines       curr linenum
applyDirs :: [String] -> Int ->         
--          rec counter  curr file   processed lines
            Int ->       String ->   [IO String]
applyDirs [] ln r cf = []
applyDirs (s:ss) ln r cf | isDirective s = 
    let s' = case getDirectiveType s of {
        -- every one of these must `return` or yield an `IO String`
        "include" ->
            case parse includeModuleDir "" s of
                -- if not a module import, probably a relative import
                Left _ -> case parse includeRelDir "" s of
                    Left _   -> parseFail s ln
                    Right s' -> do
                        cf' <- getFromRel cf s'
                        s'' <- getFileText cf'
                        -- reprocess in case the included file also has imports
                        process s'' (r+1) cf'
                -- yields an `IO String`
                Right s' -> do 
                    cf' <- getFromLib s'
                    s'' <- getFileText cf'
                    -- reprocess incase the included file also has imports
                    process s'' (r+1) cf'
        ;
        _ -> parseFail s ln
        }
    in s' : applyDirs ss (ln+1) r cf
applyDirs (s:ss) ln r cf = (return s) : applyDirs ss (ln+1) r cf


--
--- Parser stuff
-- 

includeModuleDir :: Parser String
includeModuleDir = do
    many whiteSpace
    string "#include"
    many1 whiteSpace
    char '<'
    f <- many1 absFilenameChar
    char '>'
    many whiteSpace
    eof
    return f

includeRelDir :: Parser String
includeRelDir = do
    many whiteSpace
    string "#include"
    many1 whiteSpace
    char '"'
    f <- many1 relFilenameChar
    char '"'
    many whiteSpace
    eof
    return f

absNonFileChars = Set.fromList ['<','>']
absFilenameChar = satisfy (\c -> not $ c `Set.member` absNonFileChars)

relNonFileChars = Set.fromList ['"','"']
relFilenameChar = satisfy (\c -> not $ c `Set.member` relNonFileChars)

whiteSpace = char ' '

-- true if the given line is a preprocessor directive
isDirective line = case trim line of
    '#':_ -> True
    _ -> False

getDirectiveType line = case parse directiveType "" line of
    Left msg -> error $ "Could not parse directive:\n" ++ line
    Right d -> d

directiveType :: Parser String
directiveType = do
    many whiteSpace
    char '#'
    d <- many1 alphaNum
    return d
