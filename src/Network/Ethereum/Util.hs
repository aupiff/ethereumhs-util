{-# LANGUAGE OverloadedStrings #-}

module Network.Ethereum.Util where

import           Crypto.Hash (Digest, Keccak_256, hash)
import           Crypto.Secp256k1
import           Data.ByteString.Short (ShortByteString, fromShort, toShort)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as BS16
import           Data.Maybe (listToMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Word8

import           Debug.Trace

-- TODO make this function safe; decode can fail silently and that's no good
bytesDecode :: Text -> B.ByteString
bytesDecode = fst . BS16.decode . T.encodeUtf8


decomposeSig :: Text -> Maybe CompactRecSig
decomposeSig sig
        | T.length sig /= 130 = Nothing
        | otherwise = traceShow sig $ CompactRecSig sigS sigR <$> sigV -- TODO why are r & s switched?
    where sigR = toShort . (\x -> traceShow (BS16.encode x) x) . bytesDecode $ T.take 64 sig
          sigS = toShort . (\x -> traceShow (BS16.encode x) x) . bytesDecode . T.take 64 . T.drop 64 $ sig
          sigV = fmap adjustV . listToMaybe . B.unpack . bytesDecode . T.take 2 . T.drop 128 $ sig
          adjustV x | x < 27    = x
                    | otherwise = x - 27

-- TODO find a library that implements this
maybeToEither :: b -> Maybe a -> Either b a
maybeToEither msg (Just a) = Right a
maybeToEither msg Nothing = Left msg

ecrecover :: Text -> Text -> Either String (PubKey, Text, Text, Text, Text)
ecrecover sig message = do
    compactRecSig <- maybeToEither "decompose sig error" $ decomposeSig sig
    recSig <- maybeToEither "importCompactRecSig error" $ importCompactRecSig compactRecSig
    m <- maybeToEither "msg error" . msg . bytesDecode $ message
    pubkey <- maybeToEither "recover error" $ traceShow (recSig, m) $ recover recSig m
    -- hash pubkey
    let keyHash = T.pack $ show (hash (exportPubKey False pubkey) :: Digest Keccak_256)
    let keyHash2 = T.pack $ show (hash (exportPubKey True pubkey) :: Digest Keccak_256)
    -- take last 160 bits of pubkey
    let ethAddr = T.drop 24 keyHash
    let ethAddr2 = T.drop 24 keyHash2
    return (pubkey, keyHash, ethAddr, keyHash2, ethAddr2)


hashPersonalMessage :: Text -> Text
hashPersonalMessage m = T.pack . show $ (hash concatenatedString :: Digest Keccak_256)
    where messageBytes = bytesDecode m
          prefixString = "\EMEthereum Signed Message:\n" ++ (show $ B.length messageBytes)
          prefix = T.encodeUtf8 . T.pack $ prefixString
          concatenatedString = prefix `B.append` messageBytes


test = ecrecover sigT $ hashPersonalMessage messageT
    where sigT = "819df6d812858e093b28f001e5d85527cf72dcc2c5ba478bb78ca73ef96449f92f0865223bb54e0b8d7fdcccc0e4cc9bb63cb65259502d7f6c6fbcfb82cb485b1c"
          messageT = "8db36fe7023731c87ba645cab36ea211f224fe1dc38f27d0708c5d6218f3a492"
          pmessageT = "49de9e6a08cc856ae51c8b78358756379f75079edcbca21133c32d88d4075d4f"
