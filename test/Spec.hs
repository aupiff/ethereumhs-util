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
main = defaultMain tests


tests :: [Test]
tests =
    [ testGroup "hashPersonalMessage"
        [ testCase "Hashing personal message" hashPersonalMessageTest
        ]
    , testGroup "hashText"
        [ testCase "hashing message" hashTest
        ]
    , testGroup "publicToAddress"
        [ testCase "Extracting address from public key" publicToAddressTest
        ]
    , testGroup "privateToAddress"
        [ testCase "Extracting address from public key" privateToAddressTest
        ]
    , testGroup "ecsign"
        [ testCase "Signing message hashed with Ethereum prefix" ecsignTest
        , testCase "Signing 2nd message" ecsignTest2
        ]
    , testGroup "ecrecover"
        [ testCase "Recovering signature" ecrecoverTest
        ]
    ]


hashPersonalMessageTest :: Assertion
hashPersonalMessageTest =
    assertEqual "personal message hashes match" expected (hashPersonalMessage message)
  where
    message = "8db36fe7023731c87ba645cab36ea211f224fe1dc38f27d0708c5d6218f3a492"
    expected = "49de9e6a08cc856ae51c8b78358756379f75079edcbca21133c32d88d4075d4f"


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

privateToAddressTest :: Assertion
privateToAddressTest = assertEqual "appropriate address is derived from private key"
                                  (privateToAddress privKey)
                                  (Just address)
    where privKey = "024f55d169862624eec05be973a38f52ad252b3bcc0f0ed1927defa4ab4ea100"
          address = "11edd217a875063583dd1b638d16810c5d34d54b"


ecsignTest :: Assertion
ecsignTest = assertEqual "msg is properly signed after hashing"
                         (ecsign (hashPersonalMessage message) signerPrivKey)
                         (Right "819df6d812858e093b28f001e5d85527cf72dcc2c5ba478bb78ca73ef96449f92f0865223bb54e0b8d7fdcccc0e4cc9bb63cb65259502d7f6c6fbcfb82cb485b1c")
    where message = "8db36fe7023731c87ba645cab36ea211f224fe1dc38f27d0708c5d6218f3a492"
          signerPrivKey = "024f55d169862624eec05be973a38f52ad252b3bcc0f0ed1927defa4ab4ea101"

ecsignTest2 :: Assertion
ecsignTest2 = assertEqual "msg is properly signed"
                          (ecsign message signerPrivKey)
                          (Right "99e71a99cb2270b8cac5254f9e99b6210c6c10224a1579cf389ef88b20a1abe9129ff05af364204442bdb53ab6f18a99ab48acc9326fa689f228040429e3ca661b")
    where message = "82ff40c0a986c6a5cfad4ddf4c3aa6996f1a7837f9c398e17e5de5cbd5a12b28"
          signerPrivKey = "3c9229289a6125f7fdf1885a77bb12c37a8d3b4962d936f7e3084dece32a3ca1"

hashTest :: Assertion
hashTest = assertEqual "msg is properly hashed"
                       (hashText "7624778dedc75f8b322b9fa1632a610d40b85e106c7d9bf0e743a9ce291b9c6f6a362e5cee1cf5a5408ff1e12b0bc546618dffcb11edd217a875063583dd1b638d16810c5d34d54b000000000000000000000000000000000000000000000000000000000000000a0000000000000000000000000000000000000000000000000000000000000000")
                       "8db36fe7023731c87ba645cab36ea211f224fe1dc38f27d0708c5d6218f3a492"
