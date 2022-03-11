module Main where

import Type
import SLD
import Parser
import Substitution
import PrettyPrinting

main :: IO ()
main = do
    putStr "Welcome to your personal Prolog Interpreter!\n\n"
    putStr "Type \":h\" for help.\n"
    repl_loop (Prog []) dfs

repl_loop :: Prog -> Strategy -> IO ()
repl_loop prog strat = do
    putStr "?- "
    str <- getLine
    eval_loop prog strat str

eval_loop :: Prog -> Strategy -> String -> IO ()
eval_loop prog strat (':':str) = eval prog strat str
eval_loop prog strat str = do
    let x = parse str
    case x of
        (Left s) -> do
            putStrLn s
            repl_loop prog strat
        (Right goal) -> do
            returnResults (solveWith prog goal strat)
            repl_loop prog strat

returnResults :: [Subst] -> IO ()
returnResults [] = do
    putStr "false."
    return ()
returnResults (s:ss) = do
    putStr ((pretty s) ++ " ")
    cmd <- getLine 
    parseInput cmd ss
    return ()

parseInput :: String -> [Subst] -> IO ()
parseInput ('.':_) _ = do
    return ()
parseInput (';':_) [] = do
    putStrLn "false."
    return ()
parseInput ";" (s:ss) = returnResults ss
parseInput (';':xs) (s:ss) = do
    putStrLn (pretty s)
    parseInput xs ss
parseInput _ _ = do
    putStrLn "Error!"
    return ()

eval :: Prog -> Strategy -> String -> IO ()
eval prog strat ('h':_) = do
    putStrLn "<goal>      Solves/proves the specified goal."
    putStrLn ":h          Shows this help message."
    putStrLn ":l <file>   Loads the specified file."
    putStrLn ":p          Prints the currently loaded program."
    putStrLn ":q          Exits the interactive environment."
    putStrLn ":s <strat>  Sets the specified search strategy"
    putStrLn "            where <strat> is one of 'dfs' or 'bfs'."
    repl_loop prog strat
eval _ _ ('q':_) = do
    putStrLn "Quit!"
eval prog _ "s dfs" = do
    putStrLn "dfs!"
    repl_loop prog dfs
--eval prog _ "s bfs" = do
--    putStrLn "bfs!"
--    repl_loop prog bfs
eval prog strat ('l':' ':file) = do
    x <- parseFile file
    case x of
        (Left str) -> do
            putStrLn str
            repl_loop prog strat
        (Right p) -> do
            putStrLn "Loaded!"
            repl_loop p strat
eval prog strat ('p':_) = do
    putStrLn (pretty prog)
    repl_loop prog strat