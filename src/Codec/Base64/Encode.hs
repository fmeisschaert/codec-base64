module Codec.Base64.Encode (
    encodeBase64
)
where

import Data.ByteString qualified as B
import Data.ByteString.Internal qualified as B
import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr

foreign import ccall "base64.h encode_base64"
   encode_base64 :: Ptr CUChar -> CSize -> Ptr CUChar -> IO ()

encodeBase64 :: B.ByteString -> B.ByteString
encodeBase64 bs =
    let (fp,len) = B.toForeignPtr0 bs
        srcLen = toEnum len :: CSize
        encLen = ((len + 2) `div` 3) * 4
    in B.unsafeCreate encLen
        (\dst -> withForeignPtr fp
            (\src -> encode_base64 (castPtr src) srcLen (castPtr dst)))

