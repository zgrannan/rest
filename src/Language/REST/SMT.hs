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
{-# LANGUAGE ViewPatterns #-}

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
import qualified Data.IntMap as IM
import Data.IORef
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import System.Process
import Text.Parsec (endBy)
import Text.Parsec.Prim hiding (State)
import Text.ParserCombinators.Parsec.Char
import GHC.IO.Handle
import GHC.Stack (HasCallStack)
import Unsafe.Coerce (unsafeCoerce)

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
    And     :: S.Set (SMTExpr Bool) -> SMTExpr Bool
    Add     :: [SMTExpr Int] -> SMTExpr Int
    Or      :: S.Set (SMTExpr Bool) -> SMTExpr Bool
    Equal   :: S.Set (SMTExpr a)    -> SMTExpr Bool
    Greater :: SMTExpr Int    -> SMTExpr Int  -> SMTExpr Bool
    GTE     :: SMTExpr Int    -> SMTExpr Int  -> SMTExpr Bool
    Implies :: SMTExpr Bool   -> SMTExpr Bool -> SMTExpr Bool
    Var     :: SMTVar a       -> SMTExpr a
    Const   :: Int            -> SMTExpr Int


instance Eq (SMTExpr a) where
  And xs == And ys = xs == ys
  Add xs == Add ys = xs == ys
  Or xs == Or ys   = xs == ys
  Equal xs == Equal ys = xs == unsafeCoerce ys
  Greater t1 u1 == Greater t2 u2 = t1 == t2 && u1 == u2
  GTE t1 u1 == GTE t2 u2 = t1 == t2 && u1 == u2
  Implies t1 u1 == Implies t2 u2 = t1 == t2 && u1 == u2
  Var (SMTVar v1) == Var (SMTVar v2) = v1 == v2
  Const c1 == Const c2 = c1 == c2
  _ == _ = False

instance Ord (SMTExpr a) where
  And xs `compare` And ys = xs `compare` ys
  Add xs `compare` Add ys = xs `compare` ys
  Or xs `compare` Or ys   = xs `compare` ys
  Equal xs `compare` Equal ys = xs `compare` unsafeCoerce ys
  Greater t1 u1 `compare` Greater t2 u2 = (t1, u1) `compare` (t2, u2)
  GTE t1 u1 `compare` GTE t2 u2 = (t1, u1) `compare` (t2, u2)
  Implies t1 u1 `compare` Implies t2 u2 = (t1, u1) `compare` (t2, u2)
  Var (SMTVar v1) `compare` Var (SMTVar v2) = v1 `compare` v2
  Const c1 `compare` Const c2 = c1 `compare` c2
  And _ `compare` _ = LT
  _ `compare` And _ = GT
  Add _ `compare` _ = LT
  _ `compare` Add _ = GT
  Or _ `compare` _ = LT
  _ `compare` Or _ = GT
  Equal _ `compare` _ = LT
  _ `compare` Equal _ = GT
  Greater _ _ `compare` _ = LT
  _ `compare` Greater _ _ = GT
  GTE _ _ `compare` _ = LT
  _ `compare` GTE _ _ = GT
  Implies _ _ `compare` _ = LT
  _ `compare` Implies _ _ = GT
  Var (SMTVar _) `compare` _ = LT
  _ `compare` Var (SMTVar _) = GT

instance Hashable (SMTExpr a) where
  hashWithSalt salt (And xs) = hashWithSalt salt (0 :: Int, xs)
  hashWithSalt salt (Add xs) = hashWithSalt salt (1 :: Int, xs)
  hashWithSalt salt (Or xs) = hashWithSalt salt (2 :: Int, xs)
  hashWithSalt salt (Equal xs) = hashWithSalt salt (3 ::Int, xs)
  hashWithSalt salt (Greater t u) = hashWithSalt salt (4 :: Int, t, u)
  hashWithSalt salt (GTE t u) = hashWithSalt salt (5 :: Int, t, u)
  hashWithSalt salt (Implies t u) = hashWithSalt salt (6 :: Int, t, u)
  hashWithSalt salt (Var (SMTVar v)) = hashWithSalt salt (7 :: Int, v)
  hashWithSalt salt (Const c) = hashWithSalt salt (8 :: Int, c)

instance Show (SMTExpr a) where
  show = T.unpack . toFormula


toFormula :: SMTExpr a -> T.Text
toFormula = go 10 where
  go :: Int -> SMTExpr a -> T.Text
  go _ (And ts) | S.null ts = "⊤"
  go p (And ts) = eparens (p <= 5) $ T.intercalate " ∧ " $ map (go 5) (S.toList ts)
  go p (Add ts) = eparens (p <= 3) $ T.intercalate " + " $ map (go 3) ts
  go p (GTE t u)        = eparens (p <= 4) $ T.intercalate " ≥ " $ map (go 4) [t, u]
  go p (Greater t u)    = eparens (p <= 4) $ T.intercalate " > " $ map (go 4) [t, u]
  go _ (Var (SMTVar v)) = v
  go _ (Const c)        = T.pack (show c)
  go _ (Or ts) | S.null ts = "⊥"
  go p (Or ts) = eparens (p <= 6) $ T.intercalate " ∨ " $ map (go 6) (S.toList ts)
  go p (Equal ts)
    | S.size ts < 2 = "⊤"
    | otherwise = eparens (p <= 4) $ T.intercalate " = " $ map (go 4) (S.toList ts)
  go p (Implies t u)    = eparens (p <= 6) $ T.intercalate " ⇒ " $ map (go 6) [t, u]

  eparens True t = T.concat ["(", t, ")"]
  eparens False t = t

vars :: SMTExpr a -> S.Set T.Text
vars (And ts)        = S.unions (S.map vars ts)
vars (Add ts)        = S.unions (map vars ts)
vars (Or ts)         = S.unions (S.map vars ts)
vars (Equal ts)      = S.unions (S.map vars ts)
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
smtFalse = Or S.empty

smtTrue :: SMTExpr Bool
smtTrue  = And S.empty

-- | Returns an SMT expression that adds all elements in the list. If the list is empty,
--   returns @Const 0@.
smtAdd :: [SMTExpr Int] -> SMTExpr Int
smtAdd [] = Const 0
smtAdd ts = Add ts

smtOr :: [SMTExpr Bool] -> SMTExpr Bool
smtOr xs0 =
    let flattened = S.unions $ map unOr xs0
        xs1 = S.filter (not . isSmtFalse) flattened
     in if any isSmtTrue xs1 then
          smtTrue
        else
          let xs2 = S.map reAnd $ absorb $ S.map unAnd xs1
           in case S.size xs2 of
                0 -> smtFalse
                1 | [x] <- S.toList xs2 -> x
                _ -> Or xs2
    where
      reAnd :: S.Set (SMTExpr Bool) -> SMTExpr Bool
      reAnd xs | S.size xs == 1, [x] <- S.toList xs = x
      reAnd xs  = And xs

isSmtFalse :: SMTExpr Bool -> Bool
isSmtFalse (Or xs) = S.null xs
isSmtFalse _       = False

smtImplies :: SMTExpr Bool -> SMTExpr Bool -> SMTExpr Bool
smtImplies = Implies

smtEqual :: [SMTExpr a] -> SMTExpr Bool
smtEqual xs =
  let xs' = S.fromList xs
   in case S.size xs' of
       0 -> smtTrue
       1 -> smtTrue
       _ -> Equal xs'

unOr :: SMTExpr Bool -> S.Set (SMTExpr Bool)
unOr (Or xs) = xs
unOr x       = S.singleton x

unAnd :: SMTExpr Bool -> S.Set (SMTExpr Bool)
unAnd (And xs) = xs
unAnd x        = S.singleton x

-- | `smtAnd ts` returns an smt expression representing \( t0 \land ... \land tn \).
smtAnd :: [SMTExpr Bool] -> SMTExpr Bool
smtAnd xs0 =
    let flattened = S.unions $ map unAnd xs0
        xs1 = S.filter (not . isSmtTrue) flattened
     in if any isSmtFalse xs1 then
          smtFalse
        else
          let xs2 = S.map reOr $ absorb $ S.map unOr xs1
           in case S.size xs2 of
                0  -> smtTrue
                1 | [x] <- S.toList xs2 -> x
                _ -> And xs2
  where
    reOr :: S.Set (SMTExpr Bool) -> SMTExpr Bool
    reOr xs | S.size xs == 1, [x] <- S.toList xs = x
    reOr xs  = Or xs

-- | Implements @A && (A || B) === A@ and @A || (B && A) === A@.
--
-- The input set contains either a disjunction of conjunctions or a
-- conjunctions of disjunctions.
--
-- The output set contains the same inner sets with any elements that
-- are proper supersets of other elements removed.
--
-- For instance, if the input is @{ {A, B}, {A} }@ the output will be
-- @{ {A} }@.
--
absorb :: S.Set (S.Set (SMTExpr Bool)) -> S.Set (S.Set (SMTExpr Bool))
absorb ors =
  let orsList = S.toAscList ors
   in S.filter (\ox -> not $ any (`S.isProperSubsetOf` ox) orsList) ors


isSmtTrue :: SMTExpr Bool -> Bool
isSmtTrue (And xs) = S.null xs
isSmtTrue _        = False

-- | `smtGTE t u` returns an SMT expression \( t \geqslant u \). If @t == u@, returns 'smtTrue'.
smtGTE :: SMTExpr Int -> SMTExpr Int -> SMTExpr Bool
smtGTE t u | t == u    = smtTrue
smtGTE t u  = GTE t u

app :: T.Text -> [SMTExpr a] -> T.Text
app op trms = T.concat ["(", op, " ", T.intercalate " " (map exprString trms), ")"]

exprString :: SMTExpr a -> T.Text
exprString (And xs) | S.null xs = "true"
exprString (Add es)           = app "+" es
exprString (Or xs) | S.null xs = "false"
exprString (And   es)         = app "and" (S.toList es)
exprString (Or    es)         = app "or" (S.toList es)
exprString (Equal xs) | length xs < 2 = "true"
exprString (Equal es)         = app "=" (S.toList es)
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
      putStrLn $ "Interned: " ++ T.unpack v ++ " = " ++ T.unpack (toFormula e) ++ "\n"
      return $ Var $ SMTVar v

data SomeSMTExpr where
  -- | Holds the size, the hash of the expression, and the expression itself.
  SomeSMTExpr :: Int -> Int -> IM.IntMap Int -> SMTExpr a -> SomeSMTExpr

data CSEState = CSEState
  { -- | Map from SMTExpr hash to the amount of times it has been seen.
    cseBinds :: IM.IntMap Int
    -- | Binds discovered so far, in reverse order of discovery.
    -- The first element is the most recently discovered bind.
  , cseBindList :: [SomeSMTExpr]
  }

-- | Collects the subexpressions of an SMT expression, measuring their size,
-- and counting how many times each subexpression has been seen. The gloval counts
-- are stored in the state, and the counts per subexpression are returned in the
-- 'SomeSMTExpr' value.
--
-- This function should also take a substitution map as an extra parameter,
-- that replaces the common subexpressions with variables (in lazy knot fashion),
-- but for now we leave it incomplete.
--
-- Another missing piece is a function that selects the subexpressions that are
-- worth interning. It should favor the largest subexpressions that are seen more
-- than once, and use the per-subexpression counts to update the global counts as
-- bindings are introuced.
--
subExprs :: SMTExpr a -> State CSEState SomeSMTExpr
subExprs e0 = case e0 of
    And xs -> cseAnd xs
    Add xs -> cseAdd xs
    Or xs  -> cseOr xs
    Equal xs -> cseEqual xs
    Greater t u -> cseGreater t u
    GTE t u     -> cseGTE t u
    Implies t u -> cseImplies t u
    Var v       -> cseVar v
    Const c     -> cseConst c
  where
    cseAnd xs = do
      (sz, h, hm) <- cseSet xs
      let e = SomeSMTExpr sz h hm e0
      addBind e
      pure e
    cseAdd xs = do
      (sz, h, hm) <- cseList xs
      let e = SomeSMTExpr sz h hm e0
      addBind e
      pure e
    cseOr xs = do
      (sz, h, hm) <- cseSet xs
      let e = SomeSMTExpr sz h hm e0
      addBind e
      pure e
    cseEqual :: S.Set (SMTExpr a) -> State CSEState SomeSMTExpr
    cseEqual xs = do
      (sz, h, hm) <- cseSet xs
      let e = SomeSMTExpr sz h hm e0
      addBind e
      pure e
    cseGreater t u = do
      SomeSMTExpr sz1 h1 hm1 _ <- subExprs t
      SomeSMTExpr sz2 h2 hm2 _ <- subExprs u
      let sz = sz1 + sz2
          h = hashWithSalt h1 h2
          hm = IM.unionWith (+) hm1 hm2
      let e = SomeSMTExpr sz h hm e0
      addBind e
      pure e
    cseGTE t u = do
      SomeSMTExpr sz1 h1 hm1 _ <- subExprs t
      SomeSMTExpr sz2 h2 hm2 _ <- subExprs u
      let sz = sz1 + sz2
          h = hashWithSalt h1 h2
          hm = IM.unionWith (+) hm1 hm2
      let e = SomeSMTExpr sz h hm e0
      addBind e
      pure e
    cseImplies t u = do
      SomeSMTExpr sz1 h1 hm1 _ <- subExprs t
      SomeSMTExpr sz2 h2 hm2 _ <- subExprs u
      let sz = sz1 + sz2
          h = hashWithSalt h1 h2
          hm = IM.unionWith (+) hm1 hm2
      let e = SomeSMTExpr sz h hm e0
      addBind e
      pure e
    cseVar (SMTVar t) = do
      let h = hash t
      let e = SomeSMTExpr 1 h (IM.singleton h 1) e0
      addBind e
      pure e
    cseConst c = do
      let h = hash c
      let e = SomeSMTExpr 1 h (IM.singleton h 1) e0
      addBind e
      pure e

    cseList :: [SMTExpr a] -> State CSEState (Int, Int, IM.IntMap Int)
    cseList xs = do
      results <- mapM subExprs xs
      let sz = sum [s | SomeSMTExpr s _ _ _ <- results]
          h = foldl hashWithSalt 0 [h' | SomeSMTExpr _ h' _ _ <- results]
          hm = foldl (IM.unionWith (+)) (IM.singleton h sz) [m | SomeSMTExpr _ _ m _ <- results]
      return (sz, h, hm)

    cseSet :: S.Set (SMTExpr a) -> State CSEState (Int, Int, IM.IntMap Int)
    cseSet xs = cseList $ S.toAscList xs

    addBind :: SomeSMTExpr -> State CSEState ()
    addBind e@(SomeSMTExpr sz h _ _) = do
      st <- get
      case IM.lookup h (cseBinds st) of
        Nothing | sz > 7 -> do
          let newBinds = IM.insert h 1 (cseBinds st)
          put st
            { cseBinds = newBinds
            , cseBindList = e : cseBindList st
            }
        _  -> return ()
