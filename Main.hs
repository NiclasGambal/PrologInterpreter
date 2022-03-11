module Main where

import Type
import SLD
import Parser
import Substitution
import PrettyPrinting
import GHC.IO.Handle
import System.IO

main :: IO ()
main = do
    putStrLn "Welcome to your personal Prolog Interpreter!\n"
    putStrLn "Type \":h\" for help.\n"
    -- Main "read" loop starts with empty program.
    repl_loop (Prog []) dfs

repl_loop :: Prog -> Strategy -> IO ()
repl_loop prog strat = do
    putStr "-? "
    -- because ptherwise the prompt wont print because of disabled buffering
    hFlush stdout
    str <- getLine
    eval_loop prog strat str

eval_loop :: Prog -> Strategy -> String -> IO ()
-- A ":" starts a command which is evaluated in eval
eval_loop prog strat (':':str) = eval prog strat str
eval_loop prog strat str = do
    -- Otherwise we try to parse the string
    let x = parse str
    case x of
        -- In this case parsing went wrong and we print the error string.
        (Left s) -> do
            putStrLn s
            repl_loop prog strat
        -- In this case we sucessfully parsed the string.
        (Right goal) -> do
            -- We now try to solve the goal and print the results.
            returnResults (solveWith prog goal strat)
            repl_loop prog strat

returnResults :: [Subst] -> IO ()
-- There are no more solutions.
returnResults [] = do
    putStrLn "false."
    return ()
returnResults (s:ss) = do
    -- Prints the solutions.
    putStrLn ((pretty s) ++ ",")
    -- Same as about.
    hFlush stdout
    -- Try to parse input "; / ." from user.
    cmd <- getLine
    parseInput cmd ss
    return ()

parseInput :: String -> [Subst] -> IO ()
-- No more solutions wished.
parseInput ('.':_) _ = do
    return ()
-- No more solutions.
parseInput (';':_) [] = do
    putStrLn "false."
    return ()
-- Return the next result.
parseInput ";" s = returnResults s
-- Some illegal argument as input.
parseInput _ _ = do
    putStrLn "Error!"
    return ()

eval :: Prog -> Strategy -> String -> IO ()
-- Prints the help "file".
eval prog strat ('h':_) = do
    putStrLn "<goal>      Solves/proves the specified goal."
    putStrLn ":h          Shows this help message."
    putStrLn ":l <file>   Loads the specified file."
    putStrLn ":p          Prints the currently loaded program."
    putStrLn ":q          Exits the interactive environment."
    putStrLn ":s <strat>  Sets the specified search strategy"
    putStrLn "            where <strat> is one of 'dfs' or 'bfs'."
    repl_loop prog strat
-- Quits the program.
eval _ _ ('q':_) = do
    putStrLn "Quit!"
-- Changes the strategy for solving the goals.
eval prog _ "s dfs" = do
    putStrLn "dfs!"
    repl_loop prog dfs
eval prog _ "s bfs" = do
    putStrLn "bfs!"
    repl_loop prog bfs
-- Loads a program.
eval prog strat ('l':' ':file) = do
    -- Try to parse file to program
    x <- parseFile file
    case x of
        -- Case that the parsing failed, so we keep the old program.
        (Left str) -> do
            putStrLn str
            repl_loop prog strat
        -- Case that the parsing worked, set this program to our new one.
        (Right p) -> do
            putStrLn ("Loaded " ++ file ++ " into the Prolog Interpreter!")
            repl_loop p strat
-- Simply prints the current program.
eval prog strat ('p':_) = do
    putStrLn (pretty prog)
    repl_loop prog strat