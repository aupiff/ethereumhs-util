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

import           Debug.Trace

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
    return . T.pack . show $ signMsg privateKey' msgHash'


ecrecover :: Text -> Text -> Either String Text
ecrecover sig message = do
    compactRecSig <- maybeToEither "decompose sig error" $ decomposeSig sig
    recSig <- maybeToEither "importCompactRecSig error" $ importCompactRecSig compactRecSig
    m <- maybeToEither "msg error" . msg . bytesDecode $ message
    pubkey <- maybeToEither "recover error" $ recover recSig m
    -- hash pubkey
    let pubKey = T.decodeUtf8 . BS16.encode . B.drop 1 $ exportPubKey False pubkey
    return $ publicToAddress pubKey


publicToAddress :: Text -> Text
publicToAddress key = T.drop 24 keyHash
    where keyHash = T.pack $ show (hash (bytesDecode key) :: Digest Keccak_256)


hashPersonalMessage :: Text -> Text
hashPersonalMessage message = T.pack . show $ keccakDigest
    where messageBytes = bytesDecode message
          prefix = T.encodeUtf8 . T.pack $
            "\EMEthereum Signed Message:\n" ++ (show $ B.length messageBytes)
          keccakDigest :: Digest Keccak_256
          keccakDigest = hash (prefix `B.append` messageBytes)
