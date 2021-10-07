{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.REST.SMT where

import Control.Monad.IO.Class
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Text as T
import System.Process
import Text.Printf
import GHC.IO.Handle

newtype SMTVar = SMTVar T.Text deriving (Eq, Ord)

data SMTExpr a where
    And     :: [SMTExpr Bool] -> SMTExpr Bool
    Or      :: [SMTExpr Bool] -> SMTExpr Bool
    Equal   :: [SMTExpr a]    -> SMTExpr Bool
    Greater :: SMTExpr Int    -> SMTExpr Int  -> SMTExpr Bool
    Implies :: SMTExpr Bool   -> SMTExpr Bool -> SMTExpr Bool
    Var     :: SMTVar         -> SMTExpr a

vars :: SMTExpr a -> S.Set SMTVar
vars (And ts) = S.unions (map vars ts)
vars (Or ts) = S.unions (map vars ts)
vars (Equal ts) = S.unions (map vars ts)
vars (Greater t u) = S.union (vars t) (vars u)
vars (Var var) = S.singleton var
vars (Implies e1 e2) = S.union (vars e1) (vars e2)

data SMTCommand = SMTAssert (SMTExpr Bool) | DeclareVar SMTVar | CheckSat | Push | Pop

smtFalse = Or []
smtTrue :: SMTExpr Bool
smtTrue  = And []

app op trms = T.concat $ ["(", op, " ", (T.intercalate " " (map exprString trms)), ")"]

exprString :: SMTExpr a -> T.Text
exprString (And [])   = "true"
exprString (Or [])    = "false"
exprString (Equal xs) | length xs < 2 = "true"
exprString (And   es) = app "and" es
exprString (Or    es) = app "or" es
exprString (Equal es) = app "=" es
exprString (Greater e1 e2) = app ">" [e1, e2]
exprString (Implies e1 e2) = app "=>" [e1, e2]
exprString (Var (SMTVar var)) = var

commandString (SMTAssert expr) = app "assert" [expr]
commandString (DeclareVar (SMTVar var)) = T.concat $ ["(declare-const ", var,  " Int)"]
commandString CheckSat = "(check-sat)"
commandString Push     = "(push)"
commandString Pop      = "(pop)"

askCmds :: SMTExpr Bool -> [SMTCommand]
askCmds expr = [Push] ++ varDecls ++ [SMTAssert expr, CheckSat, Pop] where
  varDecls = map DeclareVar $ S.toList (vars expr)

type SolverHandle = (Handle, Handle)

spawnZ3 = do
  (Just stdIn, Just stdOut, _, _) <- createProcess (proc "z3" ["-in"]) {std_in = CreatePipe, std_out = CreatePipe}
  return (stdIn, stdOut)

killZ3 (stdIn, _) = hClose stdIn

withZ3 f =
  do
    z3     <- liftIO $ spawnZ3
    result <- f z3
    liftIO $ killZ3 z3
    return result

checkSat' :: (Handle,  Handle) -> SMTExpr Bool -> IO Bool
checkSat' (stdIn, stdOut) expr = do
  hPutStr stdIn prog
  hFlush stdIn
  result <- hGetLine stdOut
  return $ case result of
    "sat"   -> True
    "unsat" -> False
    other   -> error other
  where
    prog = (T.unpack $ (T.intercalate "\n" (map commandString $ askCmds expr))) ++ "\n"

checkSat :: SMTExpr Bool -> IO Bool
checkSat expr = do
  z3     <- spawnZ3
  result <- checkSat' z3 expr
  killZ3 z3
  return result

class ToSMTVar a b | a -> b where
  toSMTVar :: a -> SMTVar

class ToSMT a b where
  toSMT :: a -> SMTExpr b

instance {-# OVERLAPPABLE #-} (ToSMTVar a b) => ToSMT a b where
  toSMT :: a -> SMTExpr b
  toSMT op = Var $ toSMTVar op
