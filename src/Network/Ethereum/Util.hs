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

bytesDecode :: Text -> B.ByteString
bytesDecode = fst . BS16.decode . T.encodeUtf8


decomposeSig :: Text -> Maybe CompactRecSig
decomposeSig sig
        | T.length sig /= 130 = Nothing
        | otherwise = CompactRecSig sigS sigR <$> sigV -- why are these switched?
    where sigR = toShort . (\x -> traceShow (BS16.encode x) x) . bytesDecode $ T.take 64 sig
          sigS = toShort . (\x -> traceShow (BS16.encode x) x) . bytesDecode . T.take 64 . T.drop 64 $ sig
          sigV = fmap adjustV . listToMaybe . B.unpack . bytesDecode . T.take 2 . T.drop 128 $ sig
          adjustV x | x < 27    = x
                    | otherwise = x - 27

-- TODO find a library that implements this
maybeToEither :: b -> Maybe a -> Either b a
maybeToEither msg (Just a) = Right a
maybeToEither msg Nothing = Left msg

ecrecover :: a -> b -> Either String PubKey
ecrecover _ _ = do compactRecSig <- maybeToEither "decompose sig error" $ decomposeSig sigT
                   recSig <- maybeToEither "importCompactRecSig error" $ importCompactRecSig compactRecSig
                   m <- maybeToEither "msg error" $ msg . bytesDecode $ "8db36fe7023731c87ba645cab36ea211f224fe1dc38f27d0708c5d6218f3a492"
                   maybeToEither "recover error" $ traceShow (recSig, m) $ recover recSig m
    where sigT = "819df6d812858e093b28f001e5d85527cf72dcc2c5ba478bb78ca73ef96449f92f0865223bb54e0b8d7fdcccc0e4cc9bb63cb65259502d7f6c6fbcfb82cb485b1c"
