import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))

import Util (md5)


getDoorId = getLine


getPosition :: String -> Int
getPosition hash = read [hash !! 5]


getValue :: String -> Char
getValue hash = hash !! 6


update :: Map Int Char -> String -> Map Int Char
update password hash = 
  let
    (position, value) = (getPosition hash, getValue hash)
  in
    if Map.member position password
       then password
       else Map.insert position value password


isValid :: String -> Bool
isValid hash =
  take 5 hash == "00000" &&hash !! 5 `elem` ['0'..'7']


getPassword :: Map Int Char -> String
getPassword chars = map (chars!) [0..7]


main = do
  doorId <- getDoorId

  let
    getKey n = doorId ++ show n

    chars = foldr go (const Map.empty) [0..] Map.empty
    go n rest chars
      | length chars == 8 = chars
      | isValid hash = rest (update chars hash)
      | otherwise = rest chars

      where hash = md5 $ getKey n
  in
    getPassword chars
