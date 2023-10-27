{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module OutputTest where

import Control.Monad (replicateM)
import Data.Text.Lazy.Builder (Builder, fromText)
import Data.Time
import Data.Time.Calendar.OrdinalDate (pattern YearDay)
import Effectful (runEff)
import Effectful.Katip
import Katip
import System.IO (Handle, IOMode (..), hGetContents', openFile)
import System.Posix.Types (CPid (..))
import Test.QuickCheck.Instances ()
import Test.Tasty.QuickCheck

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

prop_EqualOutput :: Severity -> Namespace -> LogStr -> Property
prop_EqualOutput severity ns str = ioProperty $ do
  h1 <- openFile "scribeKCT.log" ReadWriteMode
  le1 <- logEnvWithScribe h1
  runKatipContextT le1 () ns $ logFM severity str
  _ <- closeScribes le1
  s1 <- hGetContents' h1
  h2 <- openFile "scribeEff.log" ReadWriteMode
  le2 <- logEnvWithScribe h2
  runEff . runKatipContextE le2 () ns $ logFM severity str
  _ <- closeScribes le2
  s2 <- hGetContents' h2
  cleanup
  return $ s1 == s2

cleanup :: IO ()
cleanup = do
  writeFile "scribeKCT.log" []
  writeFile "scribeEff.log" []

logEnvWithScribe :: Handle -> IO LogEnv
logEnvWithScribe fp = do
  let initialLE =
        LogEnv
          { _logEnvHost = "testing"
          , _logEnvPid = CPid 0
          , _logEnvApp = "testing"
          , _logEnvEnv = "testing"
          , _logEnvTimer = return $ UTCTime (YearDay 0 0) (secondsToDiffTime 0)
          , _logEnvScribes = mempty
          }
  s <- mkHandleScribe ColorIfTerminal fp (\_ -> pure True) V3
  registerScribe "test" s defaultScribeSettings initialLE
