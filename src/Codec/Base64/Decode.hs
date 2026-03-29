module Codec.Base64.Decode (
    Base64Flags,
    base64DecodeStrict,
    base64DecodeTrailing1,
    Base64Result(..),
    decodeBase64Result,
    decodeBase64,
    decodeBase64',
    decodeBase64Strict,
    decodeBase64Strict',
    breakDecodeBase64,
    breakDecodeBase64',
    breakDecodeBase64Strict,
    breakDecodeBase64Strict',
)
where

import Data.ByteString qualified as B
import Codec.Base64.Decode.Result

decodeBase64Flags :: Base64Flags -> B.ByteString -> B.ByteString
decodeBase64Flags flags = base64Result . decodeBase64Result flags

decodeBase64 :: B.ByteString -> B.ByteString
decodeBase64 = decodeBase64Flags mempty

decodeBase64' :: B.ByteString -> B.ByteString
decodeBase64' = decodeBase64Flags base64DecodeTrailing1

decodeBase64Strict :: B.ByteString -> B.ByteString
decodeBase64Strict = decodeBase64Flags base64DecodeStrict

decodeBase64Strict' :: B.ByteString -> B.ByteString
decodeBase64Strict' = decodeBase64Flags (base64DecodeTrailing1 <> base64DecodeStrict)

breakDecodeBase64Flags :: Base64Flags -> B.ByteString -> (B.ByteString,B.ByteString)
breakDecodeBase64Flags flags bs =
   let Base64Result _ _ _ processed decBS = decodeBase64Result flags bs
   in (decBS,B.drop processed bs)

breakDecodeBase64 :: B.ByteString -> (B.ByteString,B.ByteString)
breakDecodeBase64 = breakDecodeBase64Flags mempty

breakDecodeBase64' :: B.ByteString -> (B.ByteString,B.ByteString)
breakDecodeBase64' = breakDecodeBase64Flags base64DecodeTrailing1

breakDecodeBase64Strict :: B.ByteString -> (B.ByteString,B.ByteString)
breakDecodeBase64Strict = breakDecodeBase64Flags base64DecodeStrict

breakDecodeBase64Strict' :: B.ByteString -> (B.ByteString,B.ByteString)
breakDecodeBase64Strict' = breakDecodeBase64Flags (base64DecodeTrailing1 <> base64DecodeStrict)

