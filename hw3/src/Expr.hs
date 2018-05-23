{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Expr where

import Control.Monad.Catch (Exception, MonadThrow, throwM)
import Control.Monad.Reader (MonadReader, asks, liftM2, local, runReaderT)
import qualified Data.Map as Map

data Expr =
    Const Int |
    Var String |
    Add Expr Expr |
    Div Expr Expr |
    Sub Expr Expr |
    Mul Expr Expr |
    Pow Expr Expr |
    Let String Expr Expr deriving (Show)

data ExprError = CantFind String | ZeroDiv | NegativePow deriving (Eq, Show)

instance Exception ExprError

type ValMap = Map.Map String Int

data Stmt =
    Def String Expr |
    Upd String Expr |
    Wrt Expr |
    Rd  String
    deriving (Show)

data StmtError = VariableNotInScope String |
                 AlreadyDef String deriving (Eq, Show)

instance Exception StmtError

evalImpl :: (MonadReader ValMap m, MonadThrow m) => Expr -> m Int
evalImpl (Const x) = return x
evalImpl (Var s) = do
    var  <- asks (Map.lookup s)
    maybe (throwM (CantFind s)) return var
evalImpl (Add x y) = liftM2 (+) (evalImpl x) (evalImpl y)
evalImpl (Sub x y) = liftM2 (-) (evalImpl x) (evalImpl y)
evalImpl (Mul x y) = liftM2 (*) (evalImpl x) (evalImpl y)
evalImpl (Div x y) = do
    l <- evalImpl x
    r <- evalImpl y
    if r == 0
    then throwM ZeroDiv
    else return (l `quot` r)
evalImpl (Pow x y) = do
    l <- evalImpl x
    r <- evalImpl y
    if r < 0
    then throwM NegativePow
    else return (l ^ r)
evalImpl (Let s val inn) = do
    res <- evalImpl val
    local (Map.insert s res) (evalImpl inn)

eval :: (MonadThrow m) => Expr -> ValMap -> m Int
eval expr = runReaderT $ evalImpl expr
