{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Program where

import Expr (Stmt (..), StmtError (..), ValMap, eval)
import Parser (parseProgram)

import Control.Monad.Catch (MonadCatch, MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (MonadState, forM_, get, modify, runStateT)
import qualified Data.Map as Map
import Data.Maybe (isJust, isNothing)

def :: (MonadState ValMap m, MonadThrow m) => String -> Int -> m ()
def s val = do
    vars <- get
    if isNothing $ Map.lookup s vars
    then modify $ Map.insert s val
    else throwM $ AlreadyDef s

upd :: (MonadState ValMap m, MonadThrow m) => String -> Int -> m ()
upd s val = do
    vars <- get
    if isJust $ Map.lookup s vars
    then modify $ Map.insert s val
    else throwM $ VariableNotInScope s

rd :: (MonadState ValMap m) => String -> Int -> m ()
rd s val = modify $ Map.insert s val

wrt :: (MonadIO m) => Int -> m ()
wrt val = liftIO $ print val

valByStmt :: (MonadIO m, MonadState ValMap m, MonadCatch m) => Stmt -> m Int
valByStmt st = do
    vars <- get
    case st of
          (Def _ e) -> eval e vars
          (Upd _ e) -> eval e vars
          (Rd _)    -> read <$> liftIO getLine
          (Wrt e)   -> eval e vars

exec :: (MonadState ValMap m, MonadCatch m, MonadIO m) =>
    [Stmt] -> m ()
exec stmts = forM_ stmts $
        \st -> do
            val <- valByStmt st
            case st of
                Def s _ -> def s val
                Upd s _ -> upd s val
                Rd  s   -> rd  s val
                Wrt   _ -> wrt   val

runStmts :: [Stmt] -> IO ()
runStmts stmts = do
    let output = exec stmts
    fst <$> runStateT output Map.empty

runProgram :: String -> String -> IO ()
runProgram name program = do
    stmts <- parseProgram name program
    runStmts stmts
