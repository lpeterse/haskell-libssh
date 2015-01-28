{-# LANGUAGE ForeignFunctionInterface, CPP, EmptyDataDecls #-}

module SSH.LibSSH.Callbacks (
    ssh_set_server_callbacks
  ) where

import Foreign.C
import Foreign.Ptr

import SSH.LibSSH.Server

#include "libssh/callbacks.h"

data ServerCallbacks
data PubKey
data Channel

type AuthPasswordCallback
   = Ptr Session -> CString -> CString -> Ptr UserData -> IO CInt
type AuthNoneCallback
   = Ptr Session -> CString -> IO CInt
type AuthPubkeyCallback
   = Ptr Session -> CString -> Ptr PubKey -> CChar -> Ptr UserData -> IO CInt
type ServiceRequestCallback
   = Ptr Session -> CString -> Ptr UserData -> IO CInt
type ChannelOpenRequestSessionCallback
   = Ptr Session -> Ptr UserData -> IO (Ptr Channel)

foreign import ccall safe "misc.h ssh_new_server_callbacks"
  ssh_new_server_callbacks :: Ptr UserData
                           -> FunPtr AuthPasswordCallback
                           -> FunPtr AuthNoneCallback
                           -> FunPtr AuthPubkeyCallback
                           -> FunPtr ServiceRequestCallback
                           -> FunPtr ChannelOpenRequestSessionCallback
                           -> IO (Ptr ServerCallbacks)

foreign import ccall safe "libssh/server.h ssh_set_server_callbacks"
  ssh_set_server_callbacks :: Ptr Session -> Ptr ServerCallbacks -> IO CInt