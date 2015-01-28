{-# LANGUAGE ForeignFunctionInterface, CPP, EmptyDataDecls #-}

module SSH.LibSSH.Callbacks where

import Foreign.C
import Foreign.Ptr

import SSH.LibSSH.Server

#include "libssh/callbacks.h"

data ServerCallbacks
data PubKey
data Channel

type AuthPasswordCallback
   = Ptr Session -> CString -> CString -> Ptr UserData -> IO CInt

foreign import ccall "wrapper"
  wrapAuthPasswordCallback :: AuthPasswordCallback -> IO (FunPtr AuthPasswordCallback)

type AuthNoneCallback
   = Ptr Session -> CString -> Ptr UserData -> IO CInt

foreign import ccall "wrapper"
  wrapAuthNoneCallback :: AuthNoneCallback -> IO (FunPtr AuthNoneCallback)

type AuthPubkeyCallback
   = Ptr Session -> CString -> Ptr PubKey -> CChar -> Ptr UserData -> IO CInt

foreign import ccall "wrapper"
  wrapAuthPubkeyCallback :: AuthPubkeyCallback -> IO (FunPtr AuthPubkeyCallback)

type ServiceRequestCallback
   = Ptr Session -> CString -> Ptr UserData -> IO CInt

foreign import ccall "wrapper"
  wrapServiceRequestCallback :: ServiceRequestCallback -> IO (FunPtr ServiceRequestCallback)

type ChannelOpenRequestSessionCallback
   = Ptr Session -> Ptr UserData -> IO (Ptr Channel)

foreign import ccall "wrapper"
  wrapChannelOpenRequestSessionCallback :: ChannelOpenRequestSessionCallback -> IO (FunPtr ChannelOpenRequestSessionCallback)

foreign import ccall safe "misc.h ssh_new_server_callbacks"
  ssh_new_server_callbacks :: Ptr UserData
                           -> FunPtr AuthPasswordCallback
                           -> FunPtr AuthNoneCallback
                           -> FunPtr AuthPubkeyCallback
                           -> FunPtr ServiceRequestCallback
                           -> FunPtr ChannelOpenRequestSessionCallback
                           -> IO (Ptr ServerCallbacks)

foreign import ccall safe "misc.h ssh_free_server_callbacks"
  ssh_free_server_callbacks :: Ptr ServerCallbacks -> IO ()

foreign import ccall safe "libssh/server.h ssh_set_server_callbacks"
  ssh_set_server_callbacks :: Ptr Session -> Ptr ServerCallbacks -> IO CInt