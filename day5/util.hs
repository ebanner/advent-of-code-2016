module Util (md5) where

import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString as BS
import Numeric (showHex)
import Data.Word (Word8)


md5 :: String -> String
md5 s = concatMap byteToHex $ BS.unpack $ MD5.hash $ BSC.pack s
  where
    byteToHex :: Word8 -> String
    byteToHex b = let h = showHex b "" in if length h == 1 then '0':h else h
