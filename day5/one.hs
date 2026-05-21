import Util (md5)


getDoorId = getLine


main = do
  doorId <- getDoorId

  putStrLn $
    take 8
      [char | n <- [0..],
              let hash = md5 $ doorId ++ show n,
              take 5 hash == "00000",
              let char = hash !! 5]
