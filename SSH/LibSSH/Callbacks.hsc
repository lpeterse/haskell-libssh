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

-- | SSH channel eof callback. Called when a channel receives EOF
--
--   * @param session Current session handler
--   * @param channel the actual channel
--   * @param userdata Userdata to be passed to the callback function.
type ChannelEofCallback
   = Ptr Session -> Ptr Channel -> Ptr UserData -> IO CInt

-- | SSH channel close callback. Called when a channel is closed by remote peer
--
--   * @param session Current session handler
--   * @param channel the actual channel
--   * @param userdata Userdata to be passed to the callback function.
type ChannelCloseCallback
   = Ptr Session -> Ptr Channel -> Ptr UserData -> IO CInt

-- | SSH channel signal callback. Called when a channel has received a signal
--
--   * @param session Current session handler
--   * @param channel the actual channel
--   * @param signal the signal name (without the SIG prefix)
--   * @param userdata Userdata to be passed to the callback function.
type ChannelSignalCallback
   = Ptr Session -> Ptr Channel -> CChar -> Ptr UserData -> IO CInt

-- |  SSH channel exit status callback. Called when a channel has received an exit status
--
--   * @param session Current session handler
--   * @param channel the actual channel
--   * @param userdata Userdata to be passed to the callback function.
type ChannelExitStatusCallback
   = Ptr Session -> Ptr Channel -> CInt -> Ptr UserData -> IO CInt

-- |  SSH channel exit signal callback. Called when a channel has received an exit signal
--
--   * @param session Current session handler
--   * @param channel the actual channel
--   * @param signal the signal name (without the SIG prefix)
--   * @param core a boolean telling wether a core has been dumped or not
--   * @param errmsg the description of the exception
--   * @param lang the language of the description (format: RFC 3066)
--   * @param userdata Userdata to be passed to the callback function.
type ChannelExitSignalCallback
   = Ptr Session -> Ptr Channel -> CChar -> CString -> CInt -> CString -> CString -> Ptr UserData -> IO CInt

-- |  SSH channel PTY request from a client.
--
--   * @param channel the channel
--   * @param term The type of terminal emulation
--   * @param width width of the terminal, in characters
--   * @param height height of the terminal, in characters
--   * @param pxwidth width of the terminal, in pixels
--   * @param pxheight height of the terminal, in pixels
--   * @param userdata Userdata to be passed to the callback function.
--   * @returns 0 if the pty request is accepted
--   * @returns -1 if the request is denied
type ChannelPtyRequestCallback
   = Ptr Session -> Ptr Channel -> CString -> CInt -> CInt -> CInt -> CInt -> Ptr UserData -> IO CInt

-- |  SSH channel Shell request from a client.
--
--   * @param channel the channel
--   * @param userdata Userdata to be passed to the callback function.
--   * @returns 0 if the shell request is accepted
--   * @returns 1 if the request is denied
type ChannelShellRequestCallback
   = Ptr Session -> Ptr Channel -> Ptr UserData -> IO CInt

-- | SSH auth-agent-request from the client. This request is
--   sent by a client when agent forwarding is available.
--   Server is free to ignore this callback, no answer is expected.
--
--   * @param channel the channel
--   * @param userdata Userdata to be passed to the callback function.
type ChannelAuthAgentReqCallback
   = Ptr Session -> Ptr Channel -> Ptr UserData -> IO ()

-- | SSH X11 request from the client. This request is
--   sent by a client when X11 forwarding is requested(and available).
--   Server is free to ignore this callback, no answer is expected.
--
--   * @param channel the channel
--   * @param userdata Userdata to be passed to the callback function.
type ChannelX11ReqCallback
   = Ptr Session -> Ptr Channel -> CInt -> CString -> CString -> CUInt -> Ptr UserData -> IO ()

-- | SSH channel PTY windows change (terminal size) from a client.
--
--   * @param channel the channel
--   * @param width width of the terminal, in characters
--   * @param height height of the terminal, in characters
--   * @param pxwidth width of the terminal, in pixels
--   * @param pxheight height of the terminal, in pixels
--   * @param userdata Userdata to be passed to the callback function.
--   * @returns 0 if the pty request is accepted
--   * @returns -1 if the request is denied
type ChannelPtyWindowChangeCallback
   = Ptr Session -> Ptr Channel -> CInt -> CInt -> CInt -> CInt -> Ptr UserData -> IO CInt

-- |  SSH channel Exec request from a client.
--
--   * @param channel the channel
--   * @param command the shell command to be executed
--   * @param userdata Userdata to be passed to the callback function.
--   * @returns 0 if the exec request is accepted
--   * @returns 1 if the request is denied
type ChannelExecRequestCallback
   = Ptr Session -> Ptr Channel -> CString -> Ptr UserData -> IO CInt

-- |  SSH channel environment request from a client.
--
--   * @param channel the channel
--   * @param env_name name of the environment value to be set
--   * @param env_value value of the environment value to be set
--   * @param userdata Userdata to be passed to the callback function.
--   * @returns 0 if the env request is accepted
--   * @returns 1 if the request is denied
--   * @warning some environment variables can be dangerous if changed (e.g.
--              LD_PRELOAD) and should not be fulfilled.
type ChannelEnvRequestCallback
   = Ptr Session -> Ptr Channel -> CString -> CString -> Ptr UserData -> IO CInt

-- |  SSH channel subsystem request from a client.
--
--   * @param channel the channel
--   * @param subsystem the subsystem required
--   * @param userdata Userdata to be passed to the callback function.
--   * @returns 0 if the subsystem request is accepted
--   * @returns 1 if the request is denied
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
