{-# OPTIONS_GHC -Wno-orphans #-}

module OutputTest where

import Control.Monad (replicateM)
import Data.Text.Lazy.Builder (Builder, fromText)
import Effectful (runEff)
import Effectful.Katip
import System.IO (Handle, IOMode (..), hGetContents', openFile)
import Test.QuickCheck.Instances ()
import Test.Tasty.QuickCheck

import Data.Text (Text)
import Katip (closeScribes, runKatipContextT)
import Katip qualified as K

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

prop_EqualOutputLFM :: Severity -> Namespace -> LogStr -> Property
prop_EqualOutputLFM severity ns str = ioProperty $ do
  h1 <- openFile "scribeKCT.log" ReadWriteMode
  le1 <- logEnvWithScribe h1
  runKatipContextT le1 () ns $ K.logFM severity str
  _ <- closeScribes le1
  s1 <- hGetContents' h1
  h2 <- openFile "scribeEff.log" ReadWriteMode
  le2 <- logEnvWithScribe h2
  runEff . runKatipContextE le2 () ns $ logFM severity str
  _ <- closeScribes le2
  s2 <- hGetContents' h2
  cleanup
  return $ s1 == s2
prop_EqualOutputLF :: SimpleLogPayload -> Namespace -> Severity -> LogStr -> Property
prop_EqualOutputLF slp ns severity str = ioProperty $ do
  h1 <- openFile "scribeKCT.log" ReadWriteMode
  le1 <- logEnvWithScribe h1
  runKatipContextT le1 () ns $ K.logF slp ns severity str
  _ <- closeScribes le1
  s1 <- hGetContents' h1
  h2 <- openFile "scribeEff.log" ReadWriteMode
  le2 <- logEnvWithScribe h2
  runEff . runKatipContextE le2 () ns $ logF slp ns severity str
  _ <- closeScribes le2
  s2 <- hGetContents' h2
  cleanup
  return $ s1 == s2

prop_EqualOutputLMsg :: Namespace -> Severity -> LogStr -> Property
prop_EqualOutputLMsg ns severity str = ioProperty $ do
  h1 <- openFile "scribeKCT.log" ReadWriteMode
  le1 <- logEnvWithScribe h1
  runKatipContextT le1 () ns $ K.logMsg ns severity str
  _ <- closeScribes le1
  s1 <- hGetContents' h1
  h2 <- openFile "scribeEff.log" ReadWriteMode
  le2 <- logEnvWithScribe h2
  runEff . runKatipContextE le2 () ns $ logMsg ns severity str
  _ <- closeScribes le2
  s2 <- hGetContents' h2
  cleanup
  return $ s1 == s2
prop_EqualOutputLLoc :: SimpleLogPayload -> Namespace -> Severity -> LogStr -> Property
prop_EqualOutputLLoc slp ns severity str = ioProperty $ do
  h1 <- openFile "scribeKCT.log" ReadWriteMode
  le1 <- logEnvWithScribe h1
  runKatipContextT le1 () ns $ K.logLoc slp ns severity str
  _ <- closeScribes le1
  s1 <- hGetContents' h1
  h2 <- openFile "scribeEff.log" ReadWriteMode
  le2 <- logEnvWithScribe h2
  runEff . runKatipContextE le2 () ns $ logLoc slp ns severity str
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
  initialLE <- K.initLogEnv "testing" "testing"
  s <- K.mkHandleScribe ColorIfTerminal fp (\_ -> pure True) V3
  K.registerScribe "test" s defaultScribeSettings initialLE
