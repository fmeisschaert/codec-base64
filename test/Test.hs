{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad
import System.Exit
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C
import Codec.Base64

original :: B.ByteString
original = "urR4C,$9r3V87z89#t*KEXO@73j$241(3m9A+1T))OlSLD5tg2blpa5JaOTzxaGl"

expected :: B.ByteString
expected = "dXJSNEMsJDlyM1Y4N3o4OSN0KktFWE9ANzNqJDI0MSgzbTlBKzFUKSlPbFNMRDV0ZzJibHBhNUphT1R6eGFHbA=="

test_encode :: IO ()
test_encode = do
    let encoded = encodeBase64 original
        info = "expected: " ++ C.unpack expected ++ "\n" ++
               "encoded:  " ++ C.unpack encoded  ++ "\n"
    unless (encoded == expected) (die info)

test_decode :: IO ()
test_decode = do
    let decodeResult = decodeBase64Result mempty expected
        decoded = base64Result decodeResult
        info = "original: " ++ C.unpack original ++ "\n" ++
               "decoded:  " ++ C.unpack decoded  ++ "\n"
    unless (decoded == original) (die info)

test_decode1 :: B.ByteString -> Base64Flags -> IO ()
test_decode1 bs flags = do
    putStrLn ""
    let decodeResult = decodeBase64Result flags bs
        bs' = B.take (base64Processed decodeResult) bs
        decoded = base64Result decodeResult
    print bs
    print bs'
    print $ B.unpack decoded
    print (flags,base64Trailing1 decodeResult,base64InvalidChar decodeResult)

test_decode2 :: B.ByteString -> IO ()
test_decode2 bs = do
    putStr "\nIN: "
    print bs
    putStr "__: "
    print $ decodeBase64 bs
    print $ breakDecodeBase64 bs
    putStr "_': "
    print $ decodeBase64' bs
    print $ breakDecodeBase64' bs
    putStr "S_: "
    print $ decodeBase64Strict bs
    print $ breakDecodeBase64Strict bs
    putStr "S': "
    print $ decodeBase64Strict' bs
    print $ breakDecodeBase64Strict' bs

main :: IO ()
main = do
    test_encode
    test_decode
    mapM_ (test_decode1 "ABC DE" . toEnum) [0,1,2,3]
    mapM_ (test_decode1 "QUJDR;" . toEnum) [1,3]
    test_decode2 "QU;JDR"
    putStrLn ""
