{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms #-}

module Network.Ping.FFI
( ping_construct
, ping_destroy
, ping_setopt
, ping_host_add
, ping_send
, ping_host_remove
, ping_iterator_get
, ping_iterator_next
, ping_get_error
, ping_iterator_get_info
, Option(
      OPT_TIMEOUT
    , OPT_TTL
    , OPT_AF
    , OPT_DATA
    , OPT_SOURCE
    , OPT_DEVICE
    , OPT_QOS
    , OPT_MARK
  )
, packOption
, unpackOption
, Info(
    INFO_HOSTNAME
  , INFO_ADDRESS
  , INFO_FAMILY
  , INFO_LATENCY
  , INFO_SEQUENCE
  , INFO_IDENT
  , INFO_DATA
  , INFO_USERNAME
  , INFO_DROPPED
  , INFO_RECV_TTL
  , INFO_RECV_QOS
  )
, packInfo
, unpackInfo
, getInfo
) where

import Foreign
import Foreign.C
import Foreign.C.Types

newtype Option = Option { 
  packOption :: CInt
} deriving (Eq, Ord, Show)


unpackOption :: CInt -> Option
unpackOption = Option

pattern OPT_TIMEOUT :: Option
pattern OPT_TIMEOUT = Option 1

pattern OPT_TTL :: Option
pattern OPT_TTL = Option 2
pattern OPT_AF :: Option
pattern OPT_AF = Option 4
pattern OPT_DATA :: Option
pattern OPT_DATA = Option 8
pattern OPT_SOURCE :: Option
pattern OPT_SOURCE = Option 16
pattern OPT_DEVICE :: Option
pattern OPT_DEVICE = Option 32
pattern OPT_QOS :: Option
pattern OPT_QOS = Option 64
pattern OPT_MARK :: Option
pattern OPT_MARK = Option 128

newtype Info = Info {
  packInfo :: CInt
} deriving (Eq, Ord, Show)

unpackInfo :: CInt -> Info
unpackInfo = Info

pattern INFO_HOSTNAME :: Info  
pattern INFO_HOSTNAME = Info 1
pattern INFO_ADDRESS :: Info  
pattern INFO_ADDRESS = Info 2
pattern INFO_FAMILY :: Info 
pattern INFO_FAMILY = Info 3
pattern INFO_LATENCY :: Info  
pattern INFO_LATENCY = Info 4
pattern INFO_SEQUENCE :: Info  
pattern INFO_SEQUENCE = Info 5
pattern INFO_IDENT :: Info  
pattern INFO_IDENT = Info 6
pattern INFO_DATA :: Info  
pattern INFO_DATA = Info 7
pattern INFO_USERNAME :: Info  
pattern INFO_USERNAME = Info 8
pattern INFO_DROPPED :: Info 
pattern INFO_DROPPED = Info 9
pattern INFO_RECV_TTL :: Info 
pattern INFO_RECV_TTL = Info 10
pattern INFO_RECV_QOS :: Info 
pattern INFO_RECV_QOS = Info 11

newtype PingObj = PingObj (Ptr ())

newtype PingObjIter = PingObjIter (Ptr ())

-- | Allocates the memory necessary for a liboping object, 
-- initializes that memory and returns a pointer to it.
--
-- pingobj_t *ping_construct (void);
foreign import ccall unsafe
  ping_construct :: IO PingObj

-- | The ping_destroy iterates over all hosts associated with the liboping object obj,
-- closes the sockets, removes the hosts, and frees obj’s memory.
--
-- void ping_destroy (pingobj_t *obj);
foreign import ccall unsafe
  ping_destroy :: PingObj -> IO ()

-- int ping_setopt (pingobj_t *obj, int option, void *value);
foreign import ccall unsafe
  ping_setopt :: PingObj -> CInt -> Ptr () -> IO CInt

-- int ping_host_add (pingobj_t *obj, const char *host);
foreign import ccall unsafe
  ping_host_add :: PingObj -> CString -> IO CInt

-- int ping_send (pingobj_t *obj);
foreign import ccall unsafe
  ping_send :: PingObj -> IO CInt

-- int ping_host_remove (pingobj_t *obj, const char *host);
foreign import ccall unsafe
  ping_host_remove :: PingObj -> CString -> IO CInt

-- pingobj_iter_t *ping_iterator_get (pingobj_t *obj);
foreign import ccall unsafe
  ping_iterator_get :: PingObj -> IO PingObjIter

-- pingobj_iter_t *ping_iterator_next (pingobj_iter_t *iter);
foreign import ccall unsafe
  ping_iterator_next :: PingObjIter -> IO PingObjIter

-- int ping_iterator_count (pingobj_t *obj);
foreign import ccall unsafe
  ping_iterator_count :: PingObj -> CInt

-- |The ping_get_error method returns an error message indicating the last error encountered. This method is not thread safe whatsoever.
foreign import ccall unsafe
  ping_get_error :: PingObj -> IO CString

foreign import ccall unsafe
  ping_iterator_get_info :: PingObjIter -> CInt -> Ptr () -> Ptr CSize -> IO CInt


infoBufferSize :: Info -> Word64
infoBufferSize INFO_LATENCY = 8
infoBufferSize _ = error "unhandled info type"

getInfo :: PingObjIter -> Info -> IO (Either String Double)
getInfo poi info = do
  bufferPtr <- mallocBytes 8
  bufferLenPtr <- mallocBytes 8
  poke (castPtr bufferLenPtr) (CSize (infoBufferSize info))
  dgi <- ping_iterator_get_info poi 
                                (packInfo info)
                                bufferPtr
                                (castPtr bufferLenPtr)
  d <- (peek (castPtr bufferPtr) :: IO Double)
  free bufferPtr
  free bufferLenPtr
  pure $ Right d
