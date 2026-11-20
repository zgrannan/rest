{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

{-# LANGUAGE UndecidableInstances #-}

-- | This module contains functionality for creating SMTLIB expressions and interacting
--   with an SMT solver.
module Language.REST.SMT
  (
    checkSat
  , checkSat'
  , getModel
  , InternHandle
  , internSMTExpr
  , newInternState
  , parseModel
  , killZ3
  , sendCommands
  , sendCommandsNow
  , spawnZ3
  , smtAdd
  , smtAnd
  , smtEqual
  , smtImplies
  , smtOr
  , smtFalse
  , smtGTE
  , smtTrue
  , withZ3
  , SolverHandle
  , SMTExpr(Const, Greater)
  , SMTVar(..)
  , ToSMT(..)
  , ToSMTVar(..)
  , Z3Model
) where

import Control.Exception (bracket_)
import Control.Monad (unless)
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Hashable
import Data.IORef
import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Text as T
import System.Process
import Text.Parsec (endBy)
import Text.Parsec.Prim (many, parse, ParsecT, Stream)
import Text.ParserCombinators.Parsec.Char
import GHC.Generics (Generic)
import GHC.IO.Handle
import GHC.Stack (HasCallStack)

-- | A model returned by Z3 corresponding to a satisfiable
--   set of constraints. Untyped.
type Z3Model = M.Map String String

parens :: Text.Parsec.Prim.Stream s m Char => ParsecT s u m a -> ParsecT s u m a
parens p = do
  _ <- char '('
  r <- p
  _ <- char ')'
  return r

parseFunDef :: Text.Parsec.Prim.Stream s m Char => ParsecT s u m (String, String)
parseFunDef = parens $ do
  _     <- string "define-fun "
  var   <- many (noneOf " ")
  _     <- spaces
  _     <- many (noneOf " ") -- args
  _     <- spaces
  _     <- many (noneOf " ") -- type
  _     <- spaces
  value <- many (noneOf ")")
  return (var, value)

modelParser :: Text.Parsec.Prim.Stream s m Char => ParsecT s u m Z3Model
modelParser = parens $ do
  spaces
  defs <- endBy parseFunDef spaces
  return $ M.fromList defs

-- | Parses Z3's model string into a 'Z3Model'.
parseModel :: String -> Z3Model
parseModel str = case parse modelParser "" str of
  Left err -> error (show err)
  Right t  -> t

-- | An SMT variable
newtype SMTVar a = SMTVar T.Text deriving (Eq, Ord)

-- | SMTLib expressions
data SMTExpr a where
    And     :: [SMTExpr Bool] -> SMTExpr Bool
    Add     :: [SMTExpr Int]  -> SMTExpr Int
    Or      :: [SMTExpr Bool] -> SMTExpr Bool
    Equal   :: [SMTExpr a]    -> SMTExpr Bool
    Greater :: SMTExpr Int    -> SMTExpr Int  -> SMTExpr Bool
    GTE     :: SMTExpr Int    -> SMTExpr Int  -> SMTExpr Bool
    Implies :: SMTExpr Bool   -> SMTExpr Bool -> SMTExpr Bool
    Var     :: SMTVar a       -> SMTExpr a
    Const   :: Int            -> SMTExpr Int


data UntypedExpr =
    UAnd [UntypedExpr]
  | UAdd [UntypedExpr]
  | UOr  [UntypedExpr]
  | UEqual  [UntypedExpr]
  | UGreater UntypedExpr UntypedExpr
  | UGTE UntypedExpr UntypedExpr
  | UImplies UntypedExpr UntypedExpr
  | UVar T.Text
  | UConst Int
  deriving (Show, Eq, Ord, Hashable, Generic)

toUntyped :: SMTExpr a -> UntypedExpr
toUntyped (And xs) = UAnd (map toUntyped xs)
toUntyped (Add xs) = UAdd (map toUntyped xs)
toUntyped (Or xs)  = UOr (map toUntyped xs)
toUntyped (Equal xs) = UEqual (map toUntyped xs)
toUntyped (Greater t u) = UGreater (toUntyped t) (toUntyped u)
toUntyped (GTE t u) = UGTE (toUntyped t) (toUntyped u)
toUntyped (Implies t u) = UImplies (toUntyped t) (toUntyped u)
toUntyped (Var (SMTVar text)) = UVar text
toUntyped (Const i) = UConst i

instance (Eq (SMTExpr a)) where
  t == u = toUntyped t == toUntyped u

instance (Ord (SMTExpr a)) where
  t <= u = toUntyped t <= toUntyped u

instance Hashable (SMTExpr a) where
  hashWithSalt salt e = hashWithSalt salt (toUntyped e)

instance Show (SMTExpr a) where
  show = T.unpack . toFormula


toFormula :: SMTExpr a -> T.Text
toFormula = go 10 where
  go :: Int -> SMTExpr a -> T.Text
  go _ (And [])         = "⊤"
  go p (And ts)         = eparens (p <= 5) $ T.intercalate " ∧ " $ map (go 5) ts
  go p (Add ts)         = eparens (p <= 3) $ T.intercalate " + " $ map (go 3) ts
  go p (GTE t u)        = eparens (p <= 4) $ T.intercalate " ≥ " $ map (go 4) [t, u]
  go p (Greater t u)    = eparens (p <= 4) $ T.intercalate " > " $ map (go 4) [t, u]
  go _ (Var (SMTVar v)) = v
  go _ (Const c)        = T.pack (show c)
  go _ (Or [])          = "⊥"
  go p (Or ts)          = eparens (p <= 6) $ T.intercalate " ∨ " $ map (go 6) ts
  go p (Equal ts)
    | length ts < 2 = "⊤"
    | otherwise     = eparens (p <= 4) $ T.intercalate " = " $ map (go 4) ts
  go p (Implies t u)    = eparens (p <= 6) $ T.intercalate " ⇒ " $ map (go 6) [t, u]

  eparens True t = T.concat ["(", t, ")"]
  eparens False t = t

vars :: SMTExpr a -> S.Set T.Text
vars (And ts)        = S.unions (map vars ts)
vars (Add ts)        = S.unions (map vars ts)
vars (Or ts)         = S.unions (map vars ts)
vars (Equal ts)      = S.unions (map vars ts)
vars (Greater t u)   = S.union (vars t) (vars u)
vars (GTE t u)       = S.union (vars t) (vars u)
vars (Var (SMTVar var)) = S.singleton var
vars (Implies e1 e2) = S.union (vars e1) (vars e2)
vars (Const _)       = S.empty

data SMTCommand
    = SMTAssert (SMTExpr Bool)
    | DeclareVar T.Text
    | CheckSat
    | Push
    | Pop
    | DefineConst T.Text (SMTExpr Bool)

smtFalse :: SMTExpr Bool
smtFalse = Or []

smtTrue :: SMTExpr Bool
smtTrue  = And []

-- | Returns an SMT expression that adds all elements in the list. If the list is empty,
--   returns @Const 0@.
smtAdd :: [SMTExpr Int] -> SMTExpr Int
smtAdd [] = Const 0
smtAdd ts = Add ts

smtOr :: [SMTExpr Bool] -> SMTExpr Bool
smtOr xs0 =
    let xs1 = filter (not . isSmtFalse) $ concatMap flattenOr xs0
     in if any isSmtTrue xs1 then
          smtTrue
        else
          case L.nub xs1 of
            []  -> smtFalse
            [x] -> x
            xs2 -> Or xs2
  where
    flattenOr (Or xs) = xs
    flattenOr x      = [x]

isSmtFalse :: SMTExpr Bool -> Bool
isSmtFalse (Or []) = True
isSmtFalse _       = False

smtImplies :: SMTExpr Bool -> SMTExpr Bool -> SMTExpr Bool
smtImplies = Implies

smtEqual :: [SMTExpr a] -> SMTExpr Bool
smtEqual xs = case L.nub xs of
  []  -> smtTrue
  [_] -> smtTrue
  xs' -> Equal xs'

-- | `smtAnd ts` returns an smt expression representing \( t0 \land ... \land tn \).
smtAnd :: [SMTExpr Bool] -> SMTExpr Bool
smtAnd xs0 =
    let xs1 = filter (not . isSmtTrue) $ concatMap flattenAnd xs0
     in if any isSmtFalse xs1 then
          smtFalse
        else
          case L.nub xs1 of
            []  -> smtTrue
            [x] -> x
            xs2 -> And xs2
  where
    flattenAnd (And ys) = ys
    flattenAnd y       = [y]

isSmtTrue :: SMTExpr Bool -> Bool
isSmtTrue (And []) = True
isSmtTrue _        = False

-- | `smtGTE t u` returns an SMT expression \( t \geqslant u \). If @t == u@, returns 'smtTrue'.
smtGTE :: SMTExpr Int -> SMTExpr Int -> SMTExpr Bool
smtGTE t u | t == u    = smtTrue
smtGTE t u  = GTE t u

app :: T.Text -> [SMTExpr a] -> T.Text
app op trms = T.concat ["(", op, " ", T.intercalate " " (map exprString trms), ")"]

exprString :: SMTExpr a -> T.Text
exprString (And [])           = "true"
exprString (Add es)           = app "+" es
exprString (Or [])            = "false"
exprString (And   es)         = app "and" es
exprString (Or    es)         = app "or" es
exprString (Equal xs) | length xs < 2 = "true"
exprString (Equal es)         = app "=" es
exprString (Greater e1 e2)    = app ">" [e1, e2]
exprString (GTE e1 e2)        = app ">=" [e1, e2]
exprString (Implies e1 e2)    = app "=>" [e1, e2]
exprString (Var (SMTVar var)) = var
exprString (Const i)          = T.pack (show i)

commandString :: SMTCommand -> T.Text
commandString (SMTAssert expr) = app "assert" [expr]
commandString (DeclareVar var) = T.concat ["(declare-const ", var,  " Int)"]
commandString CheckSat = "(check-sat)"
commandString Push     = "(push)"
commandString Pop      = "(pop)"
commandString (DefineConst n e) = T.concat ["(define-fun ", n, " () Bool ", exprString e, ")"]

-- | The handle (stdIn, stdOut) used for interacting with Z3
type SolverHandle = (Handle, Handle)

-- | Instantiates a Z3 instance, returning the solver handle for interaction
spawnZ3 :: IO SolverHandle
spawnZ3 = do
  (Just stdIn, Just stdOut, _, _) <- createProcess (proc "z3" ["-in"]) {std_in = CreatePipe, std_out = CreatePipe}
  return (stdIn, stdOut)

-- | Kills the Z3 instance by closing the standard input stream
killZ3 :: SolverHandle -> IO ()
killZ3 (stdIn, _) = hClose stdIn

-- | @withZ3 f@ instantiates a Z3 instance, runs @f@ with that instance,
--   and then closes the instance and returns the result
withZ3 :: MonadIO m => (SolverHandle -> m b) -> m b
withZ3 f =
  do
    z3     <- liftIO spawnZ3
    result <- f z3
    liftIO $ killZ3 z3
    return result

-- | @getModel@ instructs an instantiated SMT solver to produce its model.
getModel :: Handle -> IO ()
getModel stdIn = do
  hPutStr stdIn "(get-model)\n"
  hFlush stdIn

sendCommands :: (Handle, Handle) -> [SMTCommand] -> IO ()
sendCommands (stdIn, _) cmds = do
  hPutStr stdIn $ T.unpack (T.intercalate "\n" (map commandString cmds)) ++ "\n"

sendCommandsNow :: (Handle, Handle) -> [SMTCommand] -> IO ()
sendCommandsNow hs@(stdIn, _) cmds = do
  sendCommands hs cmds
  hFlush stdIn

-- | Like 'checkSat', but emits to the SMT solver any constant declarations
-- necessary to check the expression.
checkSat' :: HasCallStack => (Handle,  Handle) -> SMTExpr Bool -> IO Bool
checkSat' hs@(_, stdOut) expr = do
  bracket_ (sendCommandsNow hs [Push]) (sendCommands hs [Pop]) $ do
    sendCommandsNow hs $ Push : askCmds expr
    result <- hGetLine stdOut
    case result of
      "sat"   -> return True
      "unsat" -> return False
      other   -> error other
  where
    askCmds :: SMTExpr Bool -> [SMTCommand]
    askCmds e =
      map DeclareVar (S.toList $ vars e) ++ [SMTAssert expr, CheckSat]

-- | @checkSat handles expr@ checks satisfiability of @expr@ in an instantiated SMT solver.
--   This is wrapped in a @push@ / @pop@, so it does not change the SMT environment
checkSat :: HasCallStack => (Handle,  Handle) -> SMTExpr Bool -> IO Bool
checkSat hs@(_, stdOut) expr = do
  bracket_
    (sendCommandsNow hs [Push, SMTAssert expr, CheckSat])
    (sendCommands hs [Pop]) $ do
      result <- hGetLine stdOut
      case result of
        "sat"   -> return True
        "unsat" -> return False
        other   -> error $ "checkSat: solver said: " ++ other

-- | This class allows elements of type @a@ to be used as SMT /vaiables/ of type @b@.
--   For example, the instance @ToSMTVar Op Int@ allows 'RuntimeTerm' operators to be
--   represented as 'Int' variables.
class ToSMTVar a b | a -> b where
  toSMTVar :: a -> SMTVar b

-- | This class allows elements of type @a@ to be used as SMT expressions of type
--   @b@
class ToSMT a b where
  toSMT :: a -> SMTExpr b

instance ToSMT Int Int where
  toSMT = Const

instance {-# OVERLAPPABLE #-} (ToSMTVar a b) => ToSMT a b where
  toSMT :: a -> SMTExpr b
  toSMT op = Var $ toSMTVar op


-- Interning ordering constraints
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Ordering constraints can grow large with the repeated applications of the
-- 'refine' function. The size of the expressions grows linearly with the length
-- of a rewrite path.
--
-- For this reason the implementation interns ordering constraints by declaring
-- a binding for the result of each call to 'refine'. This binding is sent to
-- the SMT solver.
--
-- Later calls to refine that take the result as input receive the
-- bound variable instead.
--
-- Defining the binding requires declaring in advance all the variables
-- that the expression uses, which it is done during the 'refine' call as well.
--
-- The implementation keeps a list of bindings that have been defined
-- and variables that have been declared, so duplicate declarations are
-- avoided.
--
-- An alternative that was considered before https://github.com/zgrannan/rest/pull/48
-- was to reduce the expressions by simplifications, but this replicates
-- manipulations that the SMT solver already does, there is no guarantee
-- that the expressions admit being simplified enough to bound the size,
-- and different OCAlgebras would likely have to deal with the same problem.
--


-- | A handle for interning SMT expressions. This is used to avoid sending the same
--  SMT expression to the solver multiple times, which can be inefficient.
data InternHandle = InternHandle
  { -- | Prefix to use for then names of interned expressions
    ihPrefix :: T.Text
    -- | Handle to the SMT solver to which the interned expressions are sent
  , ihSolver :: SolverHandle
    -- | State used to intern the SMT expressions produced by 'refine'
  , ihState  :: IORef InternState
  }

-- | State used to intern the SMT expressions produced by 'refine'.
data InternState = InternState
  { -- | The next integer to use for naming an interned expression
    isNext    :: Int
    -- | A map from SMT expressions to the names of their interned versions
  , isBinds :: M.Map (SMTExpr Bool) T.Text
    -- | The set of names of interned expressions
  , isBindNames :: S.Set T.Text
    -- | The set of variables of sort Int that have been declared in the SMT solver
  , isIntVars :: S.Set T.Text
  }

-- | Creates a new 'BindHandle' with the given prefix and SMT solver handle.
newInternState :: T.Text -> SolverHandle -> IO InternHandle
newInternState prefix sh = do
    InternHandle prefix sh
      <$> newIORef (InternState 0 M.empty S.empty S.empty)

-- | Interns the given SMT expression, returning a variable that represents it.
internSMTExpr :: InternHandle -> SMTExpr Bool -> IO (SMTExpr Bool)
internSMTExpr ih e = do
  st <- readIORef (ihState ih)
  case M.lookup e (isBinds st) of
    Just v  -> return $ Var $ SMTVar v
    Nothing -> do
      let newInts = vars e `S.difference` isBindNames st `S.difference` isIntVars st
      unless (S.null newInts) $
        sendCommands (ihSolver ih) (map DeclareVar (S.toList newInts))
      let n = isNext st
          v = ihPrefix ih <> T.pack (show n)
      writeIORef (ihState ih) st
        { isNext    = n + 1
        , isBinds = M.insert e v (isBinds st)
        , isBindNames = S.insert v (isBindNames st)
        , isIntVars = S.union (isIntVars st) newInts
        }
      sendCommands (ihSolver ih) [DefineConst v e]
      return $ Var $ SMTVar v


data SomeSMTExpr where
  -- | Holds the size, the hash of the expression, and the expression itself.
  SomeSMTExpr :: Int -> Int -> SMTExpr a -> SomeSMTExpr

data CSEState = CSEState
  { -- | Map from SMTExpr hash to the amount of times it has been seen.
    cseBinds :: IM.IntMap Int
    -- | Binds discovered so far, in reverse order of discovery.
    -- The first element is the most recently discovered bind.
  , cseBindList :: [SomeSMTExpr]
  }

cseSMTExpr :: SMTExpr a -> State CSEState (Int, SMTExpr a)
cseSMTExpr e0 = case e0 of
    e@(And xs) -> cseAnd xs
    e@(Add xs) -> cseAdd xs
    e@(Or xs)  -> cseOr xs
    e@(Equal xs) -> cseEqual xs
    e@(Greater t u) -> cseGreater t u
    e@(GTE t u)     -> cseGTE t u
    e@(Implies t u) -> cseImplies t u
    e@(Var _)       -> pure $ (1, e)
    e@(Const _)     -> pure $ (1, e)
  where
    cseAnd xs = do
      (size, xs') <- cseList xs
      addBind size e0
      pure (size, And xs')

    cseAdd xs = do
      (size, xs') <- cseList xs
      addBind size e0
      pure (size, Add xs')

    cseOr xs = do
      (size, xs') <- cseList xs
      addBind size e0
      pure (size, Or xs')

    cseEqual :: [SMTExpr a] -> State CSEState (Int, SMTExpr Bool)
    cseEqual xs = do
      (size, xs') <- cseList xs
      addBind size e0
      pure (size, Equal xs')

    cseGreater t u = do
      (sizeT, t') <- cseSMTExpr t
      (sizeU, u') <- cseSMTExpr u
      let size = sizeT + sizeU
      addBind size e0
      pure (size, Greater t' u')

    cseGTE t u = do
      (sizeT, t') <- cseSMTExpr t
      (sizeU, u') <- cseSMTExpr u
      let size = sizeT + sizeU
      addBind size e0
      pure (size, GTE t' u')

    cseImplies t u = do
      (sizeT, t') <- cseSMTExpr t
      (sizeU, u') <- cseSMTExpr u
      let size = sizeT + sizeU
      addBind size e0
      pure (size, Implies t' u')

    addBind size e | size > 7 = do
      st <- get
      let h = hash e
      case IM.lookup h (cseBinds st) of
        Just _  -> pure ()
        Nothing ->
          put st
            { cseBinds = IM.insertWith (+) h 1 (cseBinds st)
            , cseBindList = SomeSMTExpr size h e : cseBindList st
            }
    addBind _size _e = pure ()

    cseList :: [SMTExpr a] -> State CSEState (Int, [SMTExpr a])
    cseList xs = do
      (sizes, xs') <- unzip <$> mapM cseSMTExpr xs
      let size = sum sizes
      pure (size, xs')








