module Network.Ping
( ping
) where

import Foreign
import Foreign.C
import Foreign.C.Types

import Network.Ping.FFI

data PingRes = PingRes {
  pingResLatency :: Double
, pingResHost :: String
} deriving (Show)

ping :: String -> IO (Either String PingRes)
ping h = do
  po <- ping_construct
  host <- newCString h
  dah <- ping_host_add po host
  did_send <- ping_send po
  if did_send < 0
    then do
        cerr <- ping_get_error po
        err <- peekCString cerr
        pure $ Left err
    else do
       pit <- ping_iterator_get po
       elat <- getInfo pit INFO_LATENCY
       case elat of Left e -> pure (Left e)
                    Right lat -> pure $ Right (PingRes lat h)
