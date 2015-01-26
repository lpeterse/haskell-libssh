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

  , ssh_bind_error

  , ssh_bind_new
  , ssh_bind_free
  , ssh_bind_options_set
  , ssh_bind_options_set_log_verbosity
  , ssh_bind_listen

  -- * Higher-level functions
  , ErrorCode (..)
  , getError
  , getErrorCode
  ) where

import Control.Applicative
import Control.Exception
import Data.Bits
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

newtype SSH_LOG_E = SSH_LOG_E CInt
#{enum SSH_LOG_E, SSH_LOG_E,
    ssh_LOG_NOLOG = SSH_LOG_NOLOG,
    ssh_LOG_WARNING = SSH_LOG_WARNING,
    ssh_LOG_PROTOCOL = SSH_LOG_PROTOCOL,
    ssh_LOG_PACKET = SSH_LOG_PACKET,
    ssh_LOG_FUNCTIONS = SSH_LOG_FUNCTIONS
}

ssh_bind_options_set_log_verbosity :: Ptr SSH_BIND -> SSH_LOG_E -> IO CInt
ssh_bind_options_set_log_verbosity b (SSH_LOG_E i)
  = do ptr <- malloc
       poke ptr i
       result <- ssh_bind_options_set b ssh_BIND_OPTIONS_LOG_VERBOSITY ptr
       free ptr
       return result

foreign import ccall unsafe "libssh/server.h ssh_get_error"
  ssh_bind_error :: Ptr SSH_BIND -> IO CString

foreign import ccall unsafe "libssh/server.h ssh_get_error_code"
  ssh_bind_error_code :: Ptr SSH_BIND -> IO CInt

foreign import ccall unsafe "libssh/server.h ssh_bind_new"
  ssh_bind_new :: IO (Ptr SSH_BIND)

foreign import ccall unsafe "libssh/server.h ssh_bind_free"
  ssh_bind_free :: Ptr SSH_BIND -> IO ()

foreign import ccall unsafe "libssh/server.h ssh_bind_options_set"
  ssh_bind_options_set :: Ptr SSH_BIND -> SSH_BIND_OPTIONS_E -> Ptr a -> IO CInt

foreign import ccall unsafe "libssh/server.h ssh_bind_listen"
  ssh_bind_listen :: Ptr SSH_BIND -> IO CInt


type Server = Ptr SSH_BIND

data ErrorCode
   = NoError       -- ^ No error occured.
   | RequestDenied -- ^ The last request was denied but situation is recoverable.
   | Fatal         -- ^ A fatal error occurred. This could be an unexpected disconnection.
   deriving (Eq, Show)

-- | Retrieve the error text message from the last error.
getError     :: Server -> IO String
getError bind
  = ssh_bind_error bind >>= peekCString

-- | Retrieve the error code from the last error.
getErrorCode :: Server -> IO ErrorCode
getErrorCode bind
  = do code <- ssh_bind_error_code bind
       return $ case code of
        (#const SSH_NO_ERROR)       -> NoError
        (#const SSH_REQUEST_DENIED) -> RequestDenied
        _                           -> Fatal