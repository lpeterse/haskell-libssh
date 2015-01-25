{-# LANGUAGE ForeignFunctionInterface, CPP #-}

module SSH (
    version
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

version :: String
version  = concat
             [ show (#const LIBSSH_VERSION_MAJOR)
             , "."
             , show (#const LIBSSH_VERSION_MINOR)
             , "."
             , show (#const LIBSSH_VERSION_MICRO)
             ]

