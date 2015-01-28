{-# LANGUAGE ForeignFunctionInterface, CPP, EmptyDataDecls #-}

module SSH.LibSSH.Session (
    ssh_is_connected
  , ssh_disconnect

  , ssh_event_new
  , ssh_event_free
  , ssh_event_add_session
  , ssh_event_add_fd
  , ssh_event_dopoll
  ) where

import Foreign.C
import Foreign.Ptr

import SSH.LibSSH.Server

data Event

type Fd
   = CInt

type Events
   = CShort

type Revents
   = CInt

type EventCallback
   = Fd -> Revents -> Ptr UserData -> IO CInt

foreign import ccall safe "libssh/libssh.h ssh_is_connected"
  ssh_is_connected :: Ptr Session -> IO CInt

foreign import ccall safe "libssh/libssh.h ssh_disconnect"
  ssh_disconnect :: Ptr Session -> IO ()

foreign import ccall safe "libssh/libssh.h ssh_event_new"
  ssh_event_new :: IO (Ptr Event)

foreign import ccall safe "libssh/libssh.h ssh_event_free"
  ssh_event_free :: Ptr Event -> IO ()

foreign import ccall safe "libssh/libssh.h ssh_event_add_session"
  ssh_event_add_session :: Ptr Event -> Ptr Session -> IO CInt

foreign import ccall safe "libssh/libssh.h ssh_event_add_fd"
  ssh_event_add_fd :: Ptr Event -> Fd -> Events -> FunPtr EventCallback -> Ptr UserData -> IO CInt

foreign import ccall safe "libssh/libssh.h ssh_event_dopoll"
  ssh_event_dopoll :: Ptr Event -> CInt -> IO CInt
