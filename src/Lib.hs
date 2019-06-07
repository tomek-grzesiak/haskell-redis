{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib
    ( someFunc
    ) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Database.Redis as Redis
import Control.Monad.IO.Class
import GHC.Generics
import Data.Serialize as S
import Data.Maybe
import Data.Either.Combinators

someFunc :: IO ()
someFunc = connect defaultConnectInfo >>= dbAction


dbAction :: Connection -> IO ()
dbAction connection = runRedis connection $ do
  set "hello" $ encode (Tomek 10 10)
  set "world" $ encode (Tomek 20 20)
  hello <- Redis.get "hello1"
  world <- Redis.get "world"
  liftIO $ print (deserialize $ hello,deserialize $ world)

deserialize :: Either Reply (Maybe ByteString) -> Either Reply Tomek
deserialize result = result >>= (\y ->  deserializeTomek y)  

deserializeTomek:: Maybe ByteString -> Either Reply Tomek
deserializeTomek (Just value) =  mapLeft (\error -> SingleLine $ pack error) (S.decode value)
deserializeTomek Nothing = Left $ SingleLine "empty"

data Tomek = Tomek {
  age :: Int,
  size::Int
}
  deriving (Show, Generic)

instance Serialize Tomek