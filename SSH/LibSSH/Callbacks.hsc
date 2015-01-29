{-# LANGUAGE ForeignFunctionInterface, CPP, EmptyDataDecls #-}

module SSH.LibSSH.Callbacks where

import Foreign.C
import Foreign.Ptr

import SSH.LibSSH.Server

#include "libssh/callbacks.h"

data ServerCallbacks
data ChannelCallbacks
data PubKey
data Channel
data Data

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


------------------------------------
-- CHANELL CALLBACKS
------------------------------------

-- | SSH channel data callback. Called when data is available on a channel
--
--   * @param session Current session handler
--   * @param channel the actual channel
--   * @param data the data that has been read on the channel
--   * @param len the length of the data
--   * @param is_stderr is 0 for stdout or 1 for stderr
--   * @param userdata Userdata to be passed to the callback function.
--   * @returns number of bytes processed by the callee. The remaining bytes will
--     be sent in the next callback message, when more data is available.
type ChannelDataCallback
   = Ptr Session -> Ptr Channel -> Ptr Data -> CUInt -> CInt -> Ptr UserData -> IO CInt

type ChannelEofCallback
   = Ptr Session -> Ptr Channel -> Ptr UserData -> IO CInt

type ChannelCloseCallback
   = Ptr Session -> Ptr Channel -> Ptr UserData -> IO CInt

type ChannelSignalCallback
   = Ptr Session -> Ptr Channel -> CChar -> Ptr UserData -> IO CInt

type ChannelExitStatusCallback
   = Ptr Session -> Ptr Channel -> CInt -> Ptr UserData -> IO CInt

type ChannelExitSignalCallback
   = Ptr Session -> Ptr Channel -> CChar -> CString -> CInt -> CString -> CString -> Ptr UserData -> IO CInt

type ChannelPtyRequestCallback
   = Ptr Session -> Ptr Channel -> CString -> CInt -> CInt -> CInt -> CInt -> Ptr UserData -> IO CInt

type ChannelShellRequestCallback
   = Ptr Session -> Ptr Channel -> Ptr UserData -> IO CInt

type ChannelAuthAgentReqCallback
   = Ptr Session -> Ptr Channel -> Ptr UserData -> IO ()

type ChannelX11ReqCallback
   = Ptr Session -> Ptr Channel -> CInt -> CString -> CString -> CUInt -> Ptr UserData -> IO ()

type ChannelPtyWindowChangeCallback
   = Ptr Session -> Ptr Channel -> CInt -> CInt -> CInt -> CInt -> Ptr UserData -> IO CInt

type ChannelExecRequestCallback
   = Ptr Session -> Ptr Channel -> CString -> Ptr UserData -> IO CInt

type ChannelEnvRequestCallback
   = Ptr Session -> Ptr Channel -> CString -> CString -> Ptr UserData -> IO CInt

type ChannelSubsystemRequestCallback
   = Ptr Session -> Ptr Channel -> CString -> Ptr UserData -> IO CInt

foreign import ccall safe "misc.h ssh_new_channel_callbacks"
  ssh_new_channel_callbacks :: Ptr UserData
                           -> FunPtr ChannelDataCallback
                           -> FunPtr ChannelEofCallback
                           -> FunPtr ChannelCloseCallback
                           -> FunPtr ChannelSignalCallback
                           -> FunPtr ChannelExitStatusCallback
                           -> FunPtr ChannelExitSignalCallback
                           -> FunPtr ChannelPtyRequestCallback
                           -> FunPtr ChannelShellRequestCallback
                           -> FunPtr ChannelAuthAgentReqCallback
                           -> FunPtr ChannelX11ReqCallback
                           -> FunPtr ChannelPtyWindowChangeCallback
                           -> FunPtr ChannelExecRequestCallback
                           -> FunPtr ChannelEnvRequestCallback
                           -> FunPtr ChannelSubsystemRequestCallback
                           -> IO (Ptr ChannelCallbacks)
