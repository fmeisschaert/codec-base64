module Codec.Base64.Decode.Result (
    Base64Flags,
    base64DecodeStrict,
    base64DecodeTrailing1,
    Base64Result(..),
    decodeBase64Result
)
where

import Data.ByteString qualified as B
import Data.ByteString.Internal qualified as B
import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Utils (with)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable
import Data.Bits

data Base64Result = Base64Result { base64InvalidChar  :: Bool
                                 , base64Trailing1    :: Bool
                                 , base64PaddingEnd   :: Bool
                                 , base64Processed    :: Int
                                 , base64Result       :: B.ByteString
                                 }
  deriving (Show)

newtype Base64Flags = Base64Flags CInt
  deriving (Eq,Show,Enum)

instance Semigroup Base64Flags where
    Base64Flags x <> Base64Flags y = Base64Flags (x .|. y)

instance Monoid Base64Flags where
    mempty = toEnum 0

base64DecodeStrict :: Base64Flags
base64DecodeStrict = toEnum 1

base64DecodeTrailing1 :: Base64Flags
base64DecodeTrailing1 = toEnum 2

foreign import ccall "base64.h decode_base64"
   decode_base64 :: CInt -> Ptr CChar -> Ptr CSize -> Ptr CChar -> Ptr CSize -> IO CInt

decodeBase64Result :: Base64Flags -> B.ByteString -> Base64Result
decodeBase64Result flags bs =
    let Base64Flags c_flags = flags
        (fp,len) = B.toForeignPtr0 bs
        srcLen = toEnum len :: CSize
        maxDecLen = ((len + 3) `div` 4) * 3
        (decBS,(ret,processed)) =
            B.unsafeCreateUptoN' maxDecLen $ \dst ->
            withForeignPtr fp $ \src ->
            with srcLen $ \srcLenP ->
            alloca $ \dstLenP -> do
                r <- decode_base64 c_flags (castPtr src) srcLenP (castPtr dst) dstLenP
                srcLen' <- peek srcLenP
                dstLen' <- peek dstLenP
                return (fromEnum dstLen',(r,fromEnum srcLen'))
    in Base64Result (testBit ret 0) (testBit ret 1) (testBit ret 3) processed decBS

