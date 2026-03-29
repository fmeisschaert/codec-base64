module Codec.Base64 (
    encodeBase64,
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

import Codec.Base64.Encode
import Codec.Base64.Decode

