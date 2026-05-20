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


getDoorId = getLine


main = do
  doorId <- getDoorId

  putStrLn $
    take 8
      [char | n <- [0..],
              let hash = md5 $ doorId ++ show n,
              take 5 hash == "00000",
              let char = hash !! 5]
