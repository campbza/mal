import Control.Monad (when, mapM)
import Control.Monad.Error (throwError)
import qualified Data.Map as Map
import qualified Data.Traversable as DT

import Readline (readline, load_history)
import Types
import Reader (read_str)
import Printer (_pr_str)
import Env (Env, env_new, env_get, env_set)

-- read
mal_read :: String -> IO MalVal
mal_read str = read_str str

-- eval
eval_ast :: MalVal -> Env -> IO MalVal
eval_ast sym@(MalSymbol _) env = env_get env sym
eval_ast ast@(MalList lst) env = do
    new_lst <- mapM (\x -> (eval x env)) lst
    return $ MalList new_lst
eval_ast ast@(MalVector lst) env = do
    new_lst <- mapM (\x -> (eval x env)) lst
    return $ MalVector new_lst
eval_ast ast@(MalHashMap lst) env = do
    new_hm <- DT.mapM (\x -> (eval x env)) lst
    return $ MalHashMap new_hm
eval_ast ast env = return ast

let_bind :: Env -> [MalVal] -> IO Env
let_bind env [] = return env
let_bind env (b:e:xs) = do
    evaled <- eval e env
    x <- env_set env b evaled
    let_bind env xs

apply_ast :: MalVal -> Env -> IO MalVal
apply_ast ast@(MalList (MalSymbol "def!" : args)) env = do
    case args of
         (a1@(MalSymbol _): a2 : []) -> do
            evaled <- eval a2 env
            env_set env a1 evaled
         _ -> error $ "invalid def!"
apply_ast ast@(MalList (MalSymbol "let*" : args)) env = do
    case args of
         (MalList a1 : a2 : []) -> do
            let_env <- env_new $ Just env
            let_bind let_env a1
            eval a2 let_env
         (MalVector a1 : a2 : []) -> do
            let_env <- env_new $ Just env
            let_bind let_env a1
            eval a2 let_env
         _ -> error $ "invalid let*"
apply_ast ast@(MalList _) env = do
    el <- eval_ast ast env
    case el of
         (MalList (Func (Fn f) : rest)) ->
            f $ rest
         el ->
            error $ "invalid apply: " ++ (show el)

eval :: MalVal -> Env -> IO MalVal
eval ast env = do
    case ast of
         (MalList lst) -> apply_ast ast env
         _             -> eval_ast ast env


-- print
mal_print :: MalVal -> String
mal_print exp = show exp

-- repl
add args = case args of
    [MalNumber a, MalNumber b] -> return $ MalNumber $ a + b
    _ -> error $ "illegal arguments to +"
sub args = case args of
    [MalNumber a, MalNumber b] -> return $ MalNumber $ a - b
    _ -> error $ "illegal arguments to -"
mult args = case args of
    [MalNumber a, MalNumber b] -> return $ MalNumber $ a * b
    _ -> error $ "illegal arguments to *"
divd args = case args of
    [MalNumber a, MalNumber b] -> return $ MalNumber $ a `div` b
    _ -> error $ "illegal arguments to /"

rep :: Env -> String -> IO String
rep env line = do
    ast <- mal_read line
    exp <- eval ast env
    return $ mal_print exp

repl_loop :: Env -> IO ()
repl_loop env = do
    line <- readline "user> "
    case line of
        Nothing -> return ()
        Just "" -> repl_loop env
        Just str -> do
            out <- catchAny (rep env str) $ \e -> do
                return $ "Error: " ++ (show e)
            putStrLn out
            repl_loop env

main = do
    load_history
    repl_env <- env_new Nothing
    env_set repl_env (MalSymbol "+") $ _func add
    env_set repl_env (MalSymbol "-") $ _func sub
    env_set repl_env (MalSymbol "*") $ _func mult
    env_set repl_env (MalSymbol "/") $ _func divd
    repl_loop repl_env