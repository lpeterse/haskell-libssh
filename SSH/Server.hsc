{-# LANGUAGE ForeignFunctionInterface, CPP, EmptyDataDecls #-}

module SSH.Server (
    version

  , SSH_BIND ()
  , SSH_BIND_OPTIONS_E ()

  -- * Enum values
  , ssh_BIND_OPTIONS_BINDADDR
  , ssh_BIND_OPTIONS_BINDPORT
  , ssh_BIND_OPTIONS_BINDPORT_STR
  , ssh_BIND_OPTIONS_HOSTKEY
  , ssh_BIND_OPTIONS_DSAKEY
  , ssh_BIND_OPTIONS_RSAKEY
  , ssh_BIND_OPTIONS_BANNER
  , ssh_BIND_OPTIONS_LOG_VERBOSITY

  , ssh_LOG_NOLOG
  , ssh_LOG_WARNING
  , ssh_LOG_PROTOCOL
  , ssh_LOG_PACKET
  , ssh_LOG_FUNCTIONS

  , ssh_error
  , ssh_error_code

  , ssh_bind_new
  , ssh_bind_free
  , ssh_bind_options_set
  , ssh_bind_listen
  , ssh_bind_accept

  , ssh_new
  , ssh_free

  , ssh_get_pubkey_hash

  -- * Higher-level functions
  , ErrorCode (..)
  , LogVerbosity (..)
  , getError
  , getErrorCode
  , getBindError
  , getBindErrorCode

  , getPubkeyHash

  , setRsaKey
  , setBindPort
  , setLogVerbosity
  ) where

import Control.Applicative
import Control.Exception
import Data.Bits
import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Unsafe as BS
import Data.Typeable
import Data.Int
import Foreign.C
import Foreign.StablePtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Prelude hiding (catch)
--import Debug.Trace

#include "libssh/libssh.h"
#include "libssh/server.h"

version :: String
version  = concat
             [ show (#const LIBSSH_VERSION_MAJOR)
             , "."
             , show (#const LIBSSH_VERSION_MINOR)
             , "."
             , show (#const LIBSSH_VERSION_MICRO)
             ]

data SSH_BIND
data Session

newtype SSH_BIND_OPTIONS_E = SSH_BIND_OPTIONS_E CInt
#{enum SSH_BIND_OPTIONS_E, SSH_BIND_OPTIONS_E,
    ssh_BIND_OPTIONS_BINDADDR = SSH_BIND_OPTIONS_BINDADDR,
    ssh_BIND_OPTIONS_BINDPORT  = SSH_BIND_OPTIONS_BINDPORT,
    ssh_BIND_OPTIONS_BINDPORT_STR  = SSH_BIND_OPTIONS_BINDPORT_STR,
    ssh_BIND_OPTIONS_HOSTKEY  = SSH_BIND_OPTIONS_HOSTKEY,
    ssh_BIND_OPTIONS_DSAKEY  = SSH_BIND_OPTIONS_DSAKEY,
    ssh_BIND_OPTIONS_RSAKEY  = SSH_BIND_OPTIONS_RSAKEY,
    ssh_BIND_OPTIONS_BANNER  = SSH_BIND_OPTIONS_BANNER,
    ssh_BIND_OPTIONS_LOG_VERBOSITY  = SSH_BIND_OPTIONS_LOG_VERBOSITY
}

newtype SSH_LOG_E = SSH_LOG_E { ssh_log_e :: CInt }
#{enum SSH_LOG_E, SSH_LOG_E,
    ssh_LOG_NOLOG = SSH_LOG_NOLOG,
    ssh_LOG_WARNING = SSH_LOG_WARNING,
    ssh_LOG_PROTOCOL = SSH_LOG_PROTOCOL,
    ssh_LOG_PACKET = SSH_LOG_PACKET,
    ssh_LOG_FUNCTIONS = SSH_LOG_FUNCTIONS
}

foreign import ccall unsafe "libssh/server.h ssh_get_error"
  ssh_error :: Ptr a -> IO CString

foreign import ccall unsafe "libssh/server.h ssh_get_error_code"
  ssh_error_code :: Ptr a -> IO CInt

foreign import ccall unsafe "libssh/server.h ssh_bind_new"
  ssh_bind_new :: IO (Ptr SSH_BIND)

foreign import ccall unsafe "libssh/server.h ssh_bind_free"
  ssh_bind_free :: Ptr SSH_BIND -> IO ()

foreign import ccall unsafe "libssh/server.h ssh_bind_options_set"
  ssh_bind_options_set :: Ptr SSH_BIND -> SSH_BIND_OPTIONS_E -> Ptr a -> IO CInt

foreign import ccall unsafe "libssh/server.h ssh_bind_listen"
  ssh_bind_listen :: Ptr SSH_BIND -> IO CInt

foreign import ccall unsafe "libssh/server.h ssh_bind_accept"
  ssh_bind_accept :: Ptr SSH_BIND -> Ptr Session -> IO CInt

foreign import ccall unsafe "libssh/libssh.h ssh_get_pubkey_hash"
  ssh_get_pubkey_hash :: Ptr Session -> Ptr (Ptr CChar) -> IO CInt

foreign import ccall unsafe "libssh/libssh.h ssh_new"
  ssh_new :: IO (Ptr Session)

foreign import ccall unsafe "libssh/libssh.h ssh_free"
  ssh_free :: Ptr Session -> IO ()

type Server = Ptr SSH_BIND


data ErrorCode
   = NoError       -- ^ No error occured.
   | RequestDenied -- ^ The last request was denied but situation is recoverable.
   | Fatal         -- ^ A fatal error occurred. This could be an unexpected disconnection.
   deriving (Eq, Show)

data LogVerbosity
   = NoLog
   | Warning
   | Protocol
   | Packet
   | Functions
   deriving (Eq, Show)

getPubkeyHash :: Ptr Session -> IO String
getPubkeyHash session
  = do ptr <- malloc :: IO (Ptr (Ptr CChar))
       len <- ssh_get_pubkey_hash session ptr
       if len < 0
         then return ""
         else do cstr <- peek ptr
                 peekCStringLen (cstr, fromIntegral len)


setBindPort :: Word16 -> Server -> IO CInt
setBindPort port bind
  = do ptr <- malloc :: IO (Ptr CInt)
       poke ptr (fromIntegral port)
       result <- ssh_bind_options_set bind ssh_BIND_OPTIONS_BINDPORT ptr
       free ptr
       return result

setRsaKey :: String -> Server -> IO CInt
setRsaKey path bind
  = do ptr <- newCString path
       result <- ssh_bind_options_set bind ssh_BIND_OPTIONS_RSAKEY ptr
       free ptr
       return result

-- | Set the session logging verbosity.
setLogVerbosity ::  LogVerbosity -> Server -> IO CInt
setLogVerbosity verbosity bind
  = do ptr <- malloc :: IO (Ptr CInt)
       poke ptr $ ssh_log_e
                $ case verbosity of
                    NoLog     -> ssh_LOG_NOLOG
                    Warning   -> ssh_LOG_WARNING
                    Protocol  -> ssh_LOG_PROTOCOL
                    Packet    -> ssh_LOG_PACKET
                    Functions -> ssh_LOG_FUNCTIONS
       result <- ssh_bind_options_set bind ssh_BIND_OPTIONS_LOG_VERBOSITY ptr
       free ptr
       return result

-- | Retrieve the error text message from the last error.
getError     :: Ptr Session -> IO String
getError session
  = ssh_error session >>= peekCString

-- | Retrieve the error code from the last error.
getErrorCode :: Ptr Session -> IO ErrorCode
getErrorCode session
  = do code <- ssh_error_code session
       return $ case code of
        (#const SSH_NO_ERROR)       -> NoError
        (#const SSH_REQUEST_DENIED) -> RequestDenied
        _                           -> Fatal

-- | Retrieve the error text message from the last error.
getBindError     :: Server -> IO String
getBindError bind
  = ssh_error bind >>= peekCString

-- | Retrieve the error code from the last error.
getBindErrorCode :: Server -> IO ErrorCode
getBindErrorCode bind
  = do code <- ssh_error_code bind
       return $ case code of
        (#const SSH_NO_ERROR)       -> NoError
        (#const SSH_REQUEST_DENIED) -> RequestDenied
        _                           -> Fatal