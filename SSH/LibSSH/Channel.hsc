{-# LANGUAGE ForeignFunctionInterface, CPP, EmptyDataDecls #-}

module SSH.LibSSH.Channel where

import Foreign.C
import Foreign.Ptr

import SSH.LibSSH.Server

data Channel
data Data

foreign import ccall safe "libssh/libssh.h ssh_channel_new"
  ssh_channel_new :: IO (Ptr Channel)

foreign import ccall safe "libssh/libssh.h ssh_channel_free"
  ssh_channel_free :: Ptr Channel -> IO ()
