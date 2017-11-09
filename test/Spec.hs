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
        [ testCase "Hashing personal message" hashPersonalMessageTest
        ]
    , testGroup "importPublic"
        [ testCase "Importing public key into Ethereum format" importPublicKeyTest
        ]
    , testGroup "publicToAddress"
        [ testCase "extracting address from public key" publicToAddressTest
        ]
    , testGroup "ecrecover"
        [ testCase "Signing message" ecrecoverTest
        ]
    ]

hashPersonalMessageTest :: Assertion
hashPersonalMessageTest =
    assertEqual "personal message hashes match" expected (hashPersonalMessage message)
  where
    message = "8db36fe7023731c87ba645cab36ea211f224fe1dc38f27d0708c5d6218f3a492"
    expected = "49de9e6a08cc856ae51c8b78358756379f75079edcbca21133c32d88d4075d4f"


importPublicKeyTest :: Assertion
importPublicKeyTest = assertEqual "" 1 1


ecrecoverTest :: Assertion
ecrecoverTest = assertEqual "signer's ethereum address is recovered"
                            (Right signer)
                            (ecrecover sigT $ hashPersonalMessage message)
    where sigT = "819df6d812858e093b28f001e5d85527cf72dcc2c5ba478bb78ca73ef96449f92f0865223bb54e0b8d7fdcccc0e4cc9bb63cb65259502d7f6c6fbcfb82cb485b1c"
          message = "8db36fe7023731c87ba645cab36ea211f224fe1dc38f27d0708c5d6218f3a492"
          signer = "6a362e5cee1cf5a5408ff1e12b0bc546618dffcb"

publicToAddressTest :: Assertion
publicToAddressTest = assertEqual "appropriate address is derived from public key"
                                  (publicToAddress pubKey)
                                  address
    where pubKey = "3a443d8381a6798a70c6ff9304bdc8cb0163c23211d11628fae52ef9e0dca11a001cf066d56a8156fc201cd5df8a36ef694eecd258903fca7086c1fae7441e1d"
          address = "2f015c60e0be116b1f0cd534704db9c92118fb6a"
