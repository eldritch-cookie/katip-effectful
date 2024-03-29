{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE UndecidableInstances#-}

module OutputTest where

import Control.Monad (replicateM)
import Data.Text.Lazy.Builder (Builder, fromText)
import Effectful
import Effectful.Katip
import Test.QuickCheck.Instances ()
import Test.Tasty.QuickCheck

import Data.Text (Text)
import Katip (closeScribes)
import Katip qualified as K
import Data.IORef

getInteger :: Int -> Gen Int
getInteger s = chooseInt (0, s)

instance Arbitrary Namespace where
  arbitrary = do
    num <- sized getInteger
    Namespace <$> replicateM num arbitrary
  shrink = genericShrink
instance Arbitrary Environment where
  arbitrary = Environment <$> arbitrary
  shrink = genericShrink
instance Arbitrary Severity where
  arbitrary = chooseEnum (minBound, maxBound)
  shrink = genericShrink
instance Arbitrary Verbosity where
  arbitrary = chooseEnum (minBound, maxBound)
  shrink = genericShrink
instance Arbitrary LogStr where
  arbitrary = LogStr <$> arbitrary
  shrink = genericShrink
instance Arbitrary Builder where
  arbitrary = fromText <$> arbitrary
instance Arbitrary SimpleLogPayload where
  arbitrary = sl @Text <$> arbitrary <*> arbitrary

instance Show SimpleLogPayload where
  show = show . toObject

dummyScribe ::IORef [String] -> Scribe
dummyScribe ref = Scribe (appendIORef ref) (pure ()) (permitItem DebugS)

appendIORef ::forall a. LogItem a => IORef [String] -> Item a -> IO ()
appendIORef ref val = modifyIORef' ref (\prev -> prev <> [show . fmap toObject $ val])

prop_EqualOutputLFM :: Severity -> Namespace -> LogStr -> Property
prop_EqualOutputLFM severity ns str = ioProperty $ do
  testKatipAction ns (logFM severity str) (K.logFM severity str)
prop_EqualOutputLF :: SimpleLogPayload -> Namespace -> Severity -> LogStr -> Property
prop_EqualOutputLF slp ns severity str = ioProperty $ do
  testKatipAction ns (logF slp ns severity str) (K.logF slp ns severity str)

prop_EqualOutputLMsg :: Namespace -> Severity -> LogStr -> Property
prop_EqualOutputLMsg ns severity str = ioProperty $ do
  testKatipAction ns (logMsg ns severity str) (K.logMsg ns severity str)

testKatipAction:: Namespace -> Eff '[KatipE,IOE] () -> K.KatipContextT IO () -> IO Bool
testKatipAction ns eff katip = do
  effRef <- (newIORef mempty ::IO (IORef [String])) 
  katipRef <- (newIORef mempty :: IO (IORef [String]))
  le <- K.initLogEnv "testing" "testing"
  effLE <- K.registerScribe "test" (dummyScribe effRef) defaultScribeSettings le
  katipLE <- K.registerScribe "test" (dummyScribe katipRef) defaultScribeSettings le
  runEff . runKatipContextE effLE () ns $ eff
  K.runKatipContextT katipLE () ns $ katip
  _ <- closeScribes katipLE
  val1 <- readIORef effRef
  val2 <- readIORef katipRef
  pure $ val1 == val2

