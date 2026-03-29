module Codec.Base64.Encode.Lazy (
    encodeBase64
)
where

import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as L
import Data.ByteString.Lazy.Internal qualified as L
import Codec.Base64.Encode qualified as E

encodeBase64 :: L.ByteString -> L.ByteString
encodeBase64 L.Empty = L.Empty
encodeBase64 (L.Chunk sbs lbs) =
    let r = B.length sbs `mod` 3
        (sbs',lbs') = case r of
            2 -> let (x,y) = L.splitAt 1 lbs
                 in (B.append sbs $ L.toStrict x,y)
            1 -> let (x,y) = L.splitAt 2 lbs
                 in (B.append sbs $ L.toStrict x,y)
            _ -> (sbs,lbs)
    in L.chunk (E.encodeBase64 sbs') (encodeBase64 lbs')

