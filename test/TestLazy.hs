{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad
import System.Exit
import Data.ByteString.Lazy qualified as B
import Data.ByteString.Lazy.Char8 qualified as C
import Codec.Base64.Lazy

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

main :: IO ()
main = do
    test_encode
