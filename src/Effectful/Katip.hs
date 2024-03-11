{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
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
  startKatipE,
  startKatipContextE,

  -- * Initializing Loggers
  registerScribe,

  -- * Dropping Scribes Temporarily
  unregisterScribe,
  clearScribes,

  -- * Finalizing Scribes At Shutdown
  closeScribe,

  -- * Logging Functions
  LogStr (..),
  logStr,
  ls,
  showLS,

  -- ** Katip Logging Functions
  getLogEnv,
  localLogEnv,
  logF,
  logMsg,
  logT,
  logLoc,
  logItem,
  logKatipItem,
  logException,

  -- ** KatipContext Logging Functions
  getKatipContext,
  localKatipContext,
  getKatipNamespace,
  localKatipNamespace,
  logFM,
  logTM,
  logLocM,
  logItemM,
  logExceptionM,
  AnyLogContext,
  LogContexts,
  liftPayload,

  -- *** Temporarily Changing Log Behaviour
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
import Katip (
  --  logging

  -- logContext
  AnyLogContext,
  -- formatting
  ColorStrategy (..),
  Environment (..),
  Item (..),
  ItemFormatter,
  LogContexts,
  LogEnv (..),
  LogItem (..),
  LogStr (..),
  Namespace (..),
  PayloadSelection (..),
  -- permission
  PermitFunc,
  Scribe (..),
  ScribeSettings,
  Severity (..),
  SimpleLogPayload,
  ThreadIdText (..),
  ToObject (..),
  Verbosity (..),
  bracketFormat,
  -- removing scribe

  clearScribes,
  defaultScribeSettings,
  itemApp,
  itemEnv,
  itemHost,
  itemJson,
  itemLoc,
  itemMessage,
  itemNamespace,
  itemPayload,
  itemProcess,
  itemSeverity,
  itemThread,
  itemTime,
  jsonFormat,
  liftPayload,
  logEnvApp,
  logEnvEnv,
  logEnvHost,
  logEnvPid,
  logEnvScribes,
  logEnvTimer,
  logStr,
  ls,
  payloadObject,
  permitAND,
  permitItem,
  permitOR,
  renderSeverity,
  scribeBufferSize,
  showLS,
  sl,
  textToSeverity,
  unregisterScribe,
  _scribeBufferSize,
 )

import Data.Kind
import Data.Text (Text)
import Katip qualified as K
import Katip.Core (getLocTH)
import Language.Haskell.TH (Loc)
import Language.Haskell.TH.Lib
import System.IO (Handle)
import Unsafe.Coerce (unsafeCoerce)

-- | A Effect you can use to run logging actions. there is only one effect as we can't have duplicated instances.
type KatipE :: Effect
data KatipE m a

type instance DispatchOf KatipE = Static WithSideEffects

data instance StaticRep KatipE = MkKatipE !LogEnv !LogContexts !Namespace

-- | Run a KatipE Effect without a 'Namespace' or a 'LogContexts'. This also calls closeScribes
runKatipE :: forall es a. (IOE :> es) => LogEnv -> Eff (KatipE : es) a -> Eff es a
runKatipE l act = evalStaticRep (MkKatipE l mempty mempty) act <* liftIO (K.closeScribes l)

-- | Run a KatipE Effect with a 'Namespace' and a 'LogContexts'. this also calls closeScribes
runKatipContextE :: forall es a a1. (LogItem a, IOE :> es) => LogEnv -> a -> Namespace -> Eff (KatipE : es) a1 -> Eff es a1
runKatipContextE l pl ns act = evalStaticRep (MkKatipE l (liftPayload pl) ns) act <* liftIO (K.closeScribes l)

instance forall es. (IOE :> es, KatipE :> es) => K.Katip (Eff es) where
  getLogEnv :: (IOE :> es, KatipE :> es) => Eff es LogEnv
  getLogEnv = getLogEnv
  localLogEnv ::
    (IOE :> es, KatipE :> es) =>
    (LogEnv -> LogEnv) ->
    Eff es a ->
    Eff es a
  localLogEnv = localLogEnv
instance forall es. (IOE :> es, KatipE :> es) => K.KatipContext (Eff es) where
  getKatipContext :: Eff es LogContexts
  getKatipContext = getKatipContext
  localKatipContext ::
    (IOE :> es, KatipE :> es) =>
    (LogContexts -> LogContexts) ->
    Eff es a ->
    Eff es a
  localKatipContext = localKatipContext
  getKatipNamespace :: (IOE :> es, KatipE :> es) => Eff es Namespace
  getKatipNamespace = getKatipNamespace
  localKatipNamespace ::
    (IOE :> es, KatipE :> es) =>
    (Namespace -> Namespace) ->
    Eff es a ->
    Eff es a
  localKatipNamespace = localKatipNamespace

-- | Run a KatipE Effect without a 'Namespace' or a 'LogContexts' and creating a 'LogEnv'
startKatipE :: (IOE :> es) => Namespace -> Environment -> Eff (KatipE : es) a -> Eff es a
startKatipE ns env act = liftIO (K.initLogEnv ns env) >>= \lenv -> runKatipE lenv act

-- | Run a KatipE Effect with a 'Namespace' and a 'LogContexts' and creating a 'LogEnv'
startKatipContextE :: (IOE :> es, LogItem a) => Environment -> a -> Namespace -> Eff (KatipE : es) a1 -> Eff es a1
startKatipContextE env a ns act = liftIO (K.initLogEnv ns env) >>= \e -> runKatipContextE e a ns act

{- | Add a scribe to the list.
All future log calls will go to this scribe in addition to the others.
Writes will be buffered per the ScribeSettings to prevent slow scribes from slowing down logging.
Writes will be dropped if the buffer fills.
-}
registerScribe :: (KatipE :> es) => Text -> Scribe -> ScribeSettings -> Eff es ()
registerScribe txt scrb scrbs = do
  MkKatipE lenv lctx ns <- getStaticRep
  nlenv <- unsafeEff_ $ K.registerScribe txt scrb scrbs lenv
  putStaticRep $ MkKatipE nlenv lctx ns

-- | Finalize a scribe early. Note that it isn't necessary to call this as both 'runKatipE' and 'runKatipContextE' call 'K.closeScribes'
closeScribe :: (KatipE :> es) => Text -> Eff es ()
closeScribe name = do
  MkKatipE le lc ns <- getStaticRep
  newle <- unsafeEff_ $ K.closeScribe name le
  putStaticRep $ MkKatipE newle lc ns

getLogEnv :: forall es. (KatipE :> es) => Eff es LogEnv
getLogEnv = do
  s <- getStaticRep @KatipE
  case s of
    MkKatipE le _ _ -> return le
localLogEnv :: forall es a. (KatipE :> es) => (LogEnv -> LogEnv) -> Eff es a -> Eff es a
localLogEnv f = localStaticRep @KatipE $ \(MkKatipE le lc ns) -> MkKatipE (f le) lc ns

getKatipContext :: (KatipE :> es) => Eff es LogContexts
getKatipContext = do
  s <- getStaticRep @KatipE
  case s of
    MkKatipE _ lc _ -> return lc
localKatipContext :: forall es a. (KatipE :> es) => (LogContexts -> LogContexts) -> Eff es a -> Eff es a
localKatipContext f = localStaticRep @KatipE $ \(MkKatipE le lc ns) -> MkKatipE le (f lc) ns
getKatipNamespace :: forall es. (KatipE :> es) => Eff es Namespace
getKatipNamespace = do
  s <- getStaticRep @KatipE
  case s of
    MkKatipE _ _ ns -> return ns
localKatipNamespace :: forall es a. (KatipE :> es) => (Namespace -> Namespace) -> Eff es a -> Eff es a
localKatipNamespace f = localStaticRep @KatipE $ \(MkKatipE le lc ns) -> MkKatipE le lc $ f ns

-- | escape hatch for implementing your own scribes
unsafeEmbedIOE :: forall es a. (KatipE :> es) => ((KatipE :> es, IOE :> es) => Eff es a) -> Eff es a
unsafeEmbedIOE act = useDict (unsafeCoerce (MkDict @(KatipE :> es)) :: Dict (KatipE :> es, IOE :> es)) act

type Dict :: Constraint -> Type
data Dict a where
  MkDict :: (a) => Dict a

useDict :: forall a r. Dict a -> ((a) => r) -> r
useDict MkDict r = r

logF :: forall a es. (LogItem a, KatipE :> es) => a -> Namespace -> Severity -> LogStr -> Eff es ()
logF a ns sev logs = unsafeEmbedIOE $ K.logF a ns sev logs

logMsg :: forall es. (KatipE :> es) => Namespace -> Severity -> LogStr -> Eff es ()
logMsg ns sev logs = unsafeEmbedIOE $ K.logMsg ns sev logs

logT :: ExpQ
logT = [|\a ns sev msg -> logItem a ns (Just $(getLocTH)) sev msg|]

logLoc :: (LogItem a, KatipE :> es, HasCallStack) => a -> Namespace -> Severity -> LogStr -> Eff es ()
logLoc a ns sev logs = unsafeEmbedIOE $ K.logLoc a ns sev logs

logItem :: (LogItem a, KatipE :> es) => a -> Namespace -> Maybe Loc -> Severity -> LogStr -> Eff es ()
logItem a ns loc sev logs = unsafeEmbedIOE $ K.logItem a ns loc sev logs

logKatipItem :: (LogItem a, KatipE :> es) => Item a -> Eff es ()
logKatipItem item = unsafeEmbedIOE $ K.logKatipItem item

logException :: (LogItem a, KatipE :> es) => a -> Namespace -> Severity -> Eff es b -> Eff es b
logException a ns sev act = unsafeEmbedIOE $ K.logException a ns sev act

logFM :: (KatipE :> es) => Severity -> LogStr -> Eff es ()
logFM sev logs = unsafeEmbedIOE $ K.logFM sev logs

logTM :: ExpQ
logTM = [|logItemM (Just $(getLocTH))|]

logLocM :: (KatipE :> es, HasCallStack) => Severity -> LogStr -> Eff es ()
logLocM sev logs = unsafeEmbedIOE $ K.logLocM sev logs

logItemM :: (KatipE :> es, HasCallStack) => Maybe Loc -> Severity -> LogStr -> Eff es ()
logItemM loc sev logs = unsafeEmbedIOE $ K.logItemM loc sev logs

logExceptionM :: (KatipE :> es) => Eff es a -> Severity -> Eff es a
logExceptionM act sev = unsafeEmbedIOE $ K.logExceptionM act sev

katipAddNamespace :: (KatipE :> es) => Namespace -> Eff es a -> Eff es a
katipAddNamespace ns = localKatipNamespace (<> ns)
katipAddContext :: (KatipE :> es, LogItem i) => i -> Eff es a -> Eff es a
katipAddContext ctx = localKatipContext (<> liftPayload ctx)
katipNoLogging :: (KatipE :> es) => Eff es a -> Eff es a
katipNoLogging = localLogEnv $ \lenv -> lenv{_logEnvScribes = mempty}

mkHandleScribe :: forall es. (KatipE :> es) => ColorStrategy -> Handle -> PermitFunc -> Verbosity -> Eff es Scribe
mkHandleScribe cs h pf v = unsafeEmbedIOE $ liftIO $ K.mkHandleScribe cs h pf v
mkHandleScribeWithFormatter :: forall es. (KatipE :> es) => (forall a. (LogItem a) => ItemFormatter a) -> ColorStrategy -> Handle -> PermitFunc -> Verbosity -> Eff es Scribe
mkHandleScribeWithFormatter ifa cs h pf v = unsafeEmbedIOE $ liftIO $ K.mkHandleScribeWithFormatter ifa cs h pf v
mkFileScribe :: forall es. (KatipE :> es) => FilePath -> PermitFunc -> Verbosity -> Eff es Scribe
mkFileScribe fp pf v = unsafeEmbedIOE $ liftIO $ K.mkFileScribe fp pf v
