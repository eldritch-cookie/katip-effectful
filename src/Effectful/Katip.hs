{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module: Effectful.Katip
Description: Effect to use Katip
-}
module Effectful.Katip (
  -- * Framework Types
  Namespace (..),
  Environment (..),
  Severity (..),
  renderSeverity,
  textToSeverity,
  Verbosity (..),
  ToObject (..),
  LogItem (..),
  Item (..),
  ThreadIdText (..),
  PayloadSelection (..),
  Scribe (..),
  LogEnv (..),
  SimpleLogPayload,
  sl,
  defaultScribeSettings,
  ScribeSettings,
  scribeBufferSize,
  _scribeBufferSize,

  -- ** @lens@-Compatible Lenses
  itemApp,
  itemEnv,
  itemSeverity,
  itemThread,
  itemHost,
  itemProcess,
  itemPayload,
  itemMessage,
  itemTime,
  itemNamespace,
  itemLoc,
  logEnvHost,
  logEnvPid,
  logEnvApp,
  logEnvEnv,
  logEnvTimer,
  logEnvScribes,

  -- * Effect
  KatipE,

  -- ** Running The Effect
  runKatipE,
  runKatipContextE,

  -- * Initializing Loggers
  initLogEnv,
  registerScribe,

  -- * Dropping Scribes Temporalily
  unregisterScribe,
  clearScribes,

  -- * Finalizing Scribes At Shutdown
  closeScribes,
  closeScribe,

  -- * Logging Functions
  LogStr (..),
  logStr,
  ls,
  showLS,

  -- ** Katip Logging Functions
  Katip (..),
  logF,
  logMsg,
  logT,
  logLoc,
  logItem,
  logKatipItem,
  logException,

  -- ** KatipContext Logging Functions
  KatipContext (..),
  logFM,
  logTM,
  logLocM,
  logItemM,
  logExceptionM,
  AnyLogContext,
  LogContexts,
  liftPayload,

  -- *** Temporalily Changing Log Behaviour
  katipAddNamespace,
  katipAddContext,
  katipNoLogging,

  -- * Included Scribes
  mkHandleScribe,
  mkHandleScribeWithFormatter,
  mkFileScribe,
  ColorStrategy (..),
  ItemFormatter,
  bracketFormat,
  jsonFormat,

  -- * Tools For Implementing Scribes
  PermitFunc,
  permitAND,
  permitOR,
  permitItem,
  payloadObject,
  itemJson,
) where

import Effectful
import Effectful.Dispatch.Static
import Katip

-- | A Effect you can use to run logging actions. there is only one effect as we can't have duplicated instances.
type KatipE :: Effect
data KatipE m a

type instance DispatchOf KatipE = Static WithSideEffects

data instance StaticRep KatipE = MkKatipE !LogEnv !LogContexts !Namespace

-- | Run a KatipE Effect without a 'Namespace' or a 'LogContexts'
runKatipE :: (IOE :> es) => LogEnv -> Eff (KatipE : es) a -> Eff es a
runKatipE l = evalStaticRep (MkKatipE l mempty mempty)

-- | Run a KatipE Effect with a 'Namespace' and a 'LogContexts'
runKatipContextE :: (LogItem a, IOE :> es) => LogEnv -> a -> Namespace -> Eff (KatipE : es) a -> Eff es a
runKatipContextE l pl ns = evalStaticRep (MkKatipE l (liftPayload pl) ns)

instance (IOE :> es, KatipE :> es) => Katip (Eff es) where
  getLogEnv = do
    s <- getStaticRep @KatipE
    case s of
      MkKatipE le _ _ -> return le
  localLogEnv f = localStaticRep @KatipE $ \(MkKatipE le lc ns) -> MkKatipE (f le) lc ns

instance (IOE :> es, KatipE :> es) => KatipContext (Eff es) where
  getKatipContext = do
    s <- getStaticRep @KatipE
    case s of
      MkKatipE _ lc _ -> return lc
  localKatipContext f = localStaticRep @KatipE $ \(MkKatipE le lc ns) -> MkKatipE le (f lc) ns
  getKatipNamespace = do
    s <- getStaticRep @KatipE
    case s of
      MkKatipE _ _ ns -> return ns
  localKatipNamespace f = localStaticRep @KatipE $ \(MkKatipE le lc ns) -> MkKatipE le lc $ f ns
