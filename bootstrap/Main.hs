module Main where

import System.Environment
import System.IO

import Parse
import WellFormed
import Interp

main :: IO ()
main = do 
    args <- getArgs
    p args

p ("-e":arg:[]) = 
    run arg
p (file:[]) = do
    h <- openFile file ReadMode
    c <- hGetContents h
    run c
p _ = print "Invalid Args."

run :: String -> IO ()
run text = 
    let p = readProg text in
        if wellFormed p then
            runIter p emptyContext
        else
            error "Malformed."

-- only parses
run' :: String -> IO ()
run' text = print $ readProg text

-- iterates over the statments, runs them in order, and prints anything
-- that needs to be printed
runIter :: Seq -> Context -> IO ()
runIter ss c = 
    let (vs, c') = interp ss c in
        printValues vs

printValues :: [Value] -> IO ()
printValues (v:vs) = case v of
    VIO v -> printValue v >> printValues vs
    _ -> printValues vs
printValues ([]) = return ()

printValue :: Value -> IO ()
printValue v = case v of
    VInt i      -> putStr $ show i
    VBool b     -> putStr $ show b
    VString s   -> putStr $ s
    VChar c     -> putChar $ c
    VFloat f    -> putStr $ show f
    VTup v1 v2  -> 
        putStr "(" 
            >> printValue v1 
            >> putStr ", " 
            >> printValue v2
            >> putStr ")"
