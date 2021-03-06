{-# LANGUAGE OverloadedStrings #-}

module Network.Ethereum.Util where

import           Crypto.Hash (Digest, Keccak_256, hash)
import           Crypto.Secp256k1
import           Data.ByteString.Short (ShortByteString, fromShort, toShort)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as BS16
import           Data.Either.Utils (maybeToEither)
import           Data.Maybe (listToMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Word8

-- TODO make this function safe; decode can fail silently and that's no good
bytesDecode :: Text -> B.ByteString
bytesDecode = fst . BS16.decode . T.encodeUtf8


-- toRpcSig
decomposeSig :: Text -> Maybe CompactRecSig
decomposeSig sig
        | T.length sig /= 130 = Nothing
        | otherwise = CompactRecSig sigS sigR <$> sigV -- TODO why are r & s switched?
    where sigR = toShort . bytesDecode $ T.take 64 sig
          sigS = toShort . bytesDecode . T.take 64 $ T.drop 64 sig
          sigV = fmap adjustV . listToMaybe . B.unpack
                              . bytesDecode . T.take 2 . T.drop 128 $ sig
          adjustV x | x < 27    = x
                    | otherwise = x - 27


ecsign :: Text -> Text -> Either String Text
ecsign msgHash privateKey = do
    msgHash' <- maybeToEither "msg error" . msg . bytesDecode $ msgHash
    privateKey' <- maybeToEither "privKey error" . secKey $ bytesDecode privateKey
    return . recSigToText $ signRecMsg privateKey' msgHash'

-- TODO consider where the best place for adding 27 is
recSigToText :: RecSig -> Text
recSigToText recSig = T.decodeUtf8 . BS16.encode $ fromShort sigS `B.append` fromShort sigR `B.snoc` sigV + 27
    where (CompactRecSig sigR sigS sigV) = exportCompactRecSig recSig

ecrecover :: Text -> Text -> Either String Text
ecrecover sig message = do
    compactRecSig <- maybeToEither "decompose sig error" $ decomposeSig sig
    recSig <- maybeToEither "importCompactRecSig error" $ importCompactRecSig compactRecSig
    m <- maybeToEither "msg error" . msg . bytesDecode $ message
    pubkey <- maybeToEither "recover error" $ recover recSig m
    -- hash pubkey
    let publicKey = pubkeyToText pubkey
    return $ publicToAddress publicKey


pubkeyToText :: PubKey -> Text
pubkeyToText = T.decodeUtf8 . BS16.encode . B.drop 1 . exportPubKey False


privateToAddress :: Text -> Maybe Text
privateToAddress = fmap publicToAddress . privateToPublic

privateToPublic :: Text -> Maybe Text
privateToPublic priv = do
    sk <- secKey $ bytesDecode priv
    return . pubkeyToText $ derivePubKey sk

publicToAddress :: Text -> Text
publicToAddress key = T.drop 24 keyHash
    where keyHash = T.pack $ show (hash (bytesDecode key) :: Digest Keccak_256)


hashPersonalMessage :: Text -> Text
hashPersonalMessage message = T.pack . show $ keccakDigest
    where messageBytes = bytesDecode message
          prefix = T.encodeUtf8 . T.pack $
            "\EMEthereum Signed Message:\n" ++ show (B.length messageBytes)
          keccakDigest :: Digest Keccak_256
          keccakDigest = hash (prefix `B.append` messageBytes)


hashText :: Text -> Text
hashText message = T.pack . show $ keccakDigest
    where keccakDigest :: Digest Keccak_256
          keccakDigest = hash $ bytesDecode message
