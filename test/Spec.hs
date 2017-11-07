{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Base16               as B16
import qualified Data.ByteString.Char8                as B8
import           Network.Ethereum.Util
import           Test.Framework                       (defaultMain, Test, testGroup)
import           Test.Framework.Providers.HUnit       (testCase)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.HUnit                           (Assertion, assertEqual)
import           Test.QuickCheck                      (Property, (==>))

main :: IO ()
main = defaultMain $ tests

tests :: [Test]
tests =
    [ testGroup "hashPersonalMessage"
        [ testCase "Signing messages" hashPersonalMessageTest
        ]
    , testGroup "ecrecover"
        [
        ]
    ]

hashPersonalMessageTest :: Assertion
hashPersonalMessageTest =
    assertEqual "personal message hashes match" expected (hashPersonalMessage message)
  where
    message = "8db36fe7023731c87ba645cab36ea211f224fe1dc38f27d0708c5d6218f3a492"
    expected = "49de9e6a08cc856ae51c8b78358756379f75079edcbca21133c32d88d4075d4f"
