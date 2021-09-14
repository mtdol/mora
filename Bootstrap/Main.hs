module Main where

import System.Environment
import System.IO
import qualified Data.Array as Array

import Parse
import Preprocessor
import WellFormed
import Desugar
import Interp

main :: IO ()
main = do 
    args <- getArgs
    handleArgs args

includePrelude = False
maxRecDepth = 700

handleArgs :: [String] -> IO ()
handleArgs ("-e":arg:[]) = 
    run True arg emptyState "" [] 0
handleArgs (file:args) = do
    h <- openFile file ReadMode
    c <- hGetContents h
    --hClose h
    run False c emptyState file args 0
handleArgs _ = print "Invalid Args."

--     interactive mode?    file text
run :: Bool ->              String ->   
--  Memory    module id   args         import count
    State ->  String ->   [String] ->  Int 
    -> IO ()
run _ _ _ _ _ ipc | ipc >= maxRecDepth =
    error "Maximum import depth exceded. Perhaps there is an import cycle."
run im text m mid args ipc = do
    -- run the preprocessor
    let (text', modData, impData) = preprocess text includePrelude
    let p = readProg text'
    -- well formed check
    let p' = case wellFormed p of {
        Right True -> p;
        Left errMsg -> error errMsg;
        }
    let p'' = desugar p'
    if im then
        let (os,m) = interpInteractive p'' mid
        in printValues os m
    -- file mode
    else 
        let (ret,os,m) = interp p'' mid args
        in printValues os m >> 
            if ret /= (VInt 0) then 
                error "Non-zero exit status." 
            else return ()

-- only parses
run' :: Bool -> String -> State -> String -> [String] -> Int
    -> IO ()
run' _ text _ _ _ _ = print $ readProg text

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
