import Debug.Trace
import Util (md5)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)


getDoorId = getLine


getPosition :: String -> Int
getPosition hash = read [hash !! 5]


getValue :: String -> Char
getValue hash = hash !! 6


update :: Map Int Char -> String -> Map Int Char
update password hash = 
  let
    (position, value) = trace ("hash=" ++ hash) $ (getPosition hash, getValue hash)
    password' = if Map.member position password
                   then password
                   else Map.insert position value password
  in
    trace ("password=" ++ show password') $ password'


isValid :: String -> Bool
isValid hash =
  take 5 hash == "00000" && hash !! 5 `elem` ['0'..'7']


toString :: Map Int Char -> String
toString chars =
  let
    password = foldl go [] [0..7]
    go password i = password ++ [chars!i]
  in
    password



main = do
  doorId <- getDoorId

  let
    getKey n = doorId ++ show n

    password = foldr go (const Map.empty) [0..] Map.empty
    go n rest password
      -- | trace ("n=" ++ show n ++ " password=" ++ show password) False = undefined
      | length password == 8 = trace "1" $ password
      | isValid hash = rest (update password hash)
      | otherwise = rest password

      where hash = md5 $ getKey n
  in
    toString password
