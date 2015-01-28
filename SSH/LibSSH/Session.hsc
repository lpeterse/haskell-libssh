{-# LANGUAGE ForeignFunctionInterface, CPP, EmptyDataDecls #-}

module SSH.LibSSH.Session (
    ssh_is_connected
  , ssh_disconnect
  ) where

import Foreign.C
import Foreign.Ptr

import SSH.LibSSH.Server

foreign import ccall safe "libssh/libssh.h ssh_is_connected"
  ssh_is_connected :: Ptr Session -> IO CInt

foreign import ccall safe "libssh/libssh.h ssh_disconnect"
  ssh_disconnect :: Ptr Session -> IO ()