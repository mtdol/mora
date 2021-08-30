module Main where

import System.Environment
import System.IO
import qualified Data.Array as Array

import Parse
--import WellFormed
import Interp

main :: IO ()
main = do 
    args <- getArgs
    p args

p ("-e":arg:[]) = 
    run True arg []
p (file:args) = do
    h <- openFile file ReadMode
    c <- hGetContents h
    run False c args
p _ = print "Invalid Args."

run :: Bool -> String -> [String] -> IO ()
run im text args = 
    let p = readProg text in
        if True {-TODO: make this a wellness check-} then
            -- interactive mode
            if im then
                let (vs,m) = interpInteractive p
                in printValues vs m
            -- file mode
            else 
                let (ret,vs,m) = interp p args 
                in printValues vs m >> 
                    if ret /= (VInt 0) then 
                        error "Non-zero exit status." 
                    else return ()
        else
            error "Malformed."

-- only parses
run' :: Bool -> String -> [String] ->IO ()
run' _ text _ = print $ readProg text

printValues :: [Value] -> State -> IO ()
printValues (v:vs) m = case v of
    VIO v -> printValue v m >> printValues vs m
    _ -> printValues vs m
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
