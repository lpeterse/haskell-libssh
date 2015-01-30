{-# LANGUAGE ForeignFunctionInterface, CPP, EmptyDataDecls #-}

module SSH.LibSSH.Log (
    LogCallback
  , wrapLogCallback
  , ssh_set_log_callback
  , ssh_set_log_level
  , ssh_log
  , _ssh_log
  ) where

import Foreign.C
import Foreign.Ptr

import SSH.LibSSH.Server

-- | > typedef void (*ssh_logging_callback) (int priority,
--   >                                       const char *function,
--   >                                       const char *buffer,
--   >                                       void *userdata);
type LogCallback
   = CInt -> CString -> CString -> Ptr UserData -> IO ()

foreign import ccall "libssh/libssh.h ssh_set_log_callback"
  ssh_set_log_callback :: FunPtr LogCallback -> IO CInt

foreign import ccall "libssh/libssh.h ssh_set_log_level"
  ssh_set_log_level :: CInt -> IO CInt

foreign import ccall "wrapper"
  wrapLogCallback :: LogCallback -> IO (FunPtr LogCallback)

foreign import ccall "libssh/libssh.h ssh_log"
  ssh_log :: Ptr Session -> CInt -> CString -> IO ()

foreign import ccall "libssh/libssh.h _ssh_log"
  _ssh_log :: CInt -> CString -> CString -> IO ()

