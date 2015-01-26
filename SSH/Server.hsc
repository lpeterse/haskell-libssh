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

  , ssh_bind_new
  , ssh_bind_free
  , ssh_bind_options_set
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
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Ptr
import Foreign.ForeignPtr
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

foreign import ccall unsafe "libssh/server.h ssh_bind_new"
  ssh_bind_new :: IO (Ptr SSH_BIND)

foreign import ccall unsafe "libssh/server.h ssh_bind_free"
  ssh_bind_free :: Ptr SSH_BIND -> IO ()

foreign import ccall unsafe "libssh/server.h ssh_bind_options_set"
  ssh_bind_options_set :: Ptr SSH_BIND -> SSH_BIND_OPTIONS_E -> Ptr () -> IO ()


