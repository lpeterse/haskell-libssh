module SSH.Server
  (
  -- * Higher-level functions
    ErrorCode (..)
  , LogVerbosity (..)
  , getError
  , getErrorCode
  , getBindError
  , getBindErrorCode

  , getPubkeyHash

  , setRsaKey
  , setBindPort
  , setLogVerbosity
  )
  where


import Data.Word

import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Ptr
import Foreign.ForeignPtr

import SSH.LibSSH.Server

type Server = Ptr Bind


data ErrorCode
   = NoError       -- ^ No error occured.
   | RequestDenied -- ^ The last request was denied but situation is recoverable.
   | Fatal         -- ^ A fatal error occurred. This could be an unexpected disconnection.
   deriving (Eq, Show)

data LogVerbosity
   = NoLog
   | Warning
   | Protocol
   | Packet
   | Functions
   deriving (Eq, Show)

getPubkeyHash :: Ptr Session -> IO String
getPubkeyHash session
  = do ptr <- malloc :: IO (Ptr (Ptr CChar))
       len <- ssh_get_pubkey_hash session ptr
       if len < 0
         then return ""
         else do cstr <- peek ptr
                 peekCStringLen (cstr, fromIntegral len)



setBindPort :: Word16 -> Server -> IO CInt
setBindPort port bind
  = do ptr <- malloc :: IO (Ptr CInt)
       poke ptr (fromIntegral port)
       result <- ssh_bind_options_set bind ssh_BIND_OPTIONS_BINDPORT ptr
       free ptr
       return result

setRsaKey :: String -> Server -> IO CInt
setRsaKey path bind
  = do ptr <- newCString path
       result <- ssh_bind_options_set bind ssh_BIND_OPTIONS_RSAKEY ptr
       free ptr
       return result

-- | Set the session logging verbosity.
setLogVerbosity ::  LogVerbosity -> Server -> IO CInt
setLogVerbosity verbosity bind
  = do ptr <- malloc :: IO (Ptr CInt)
       poke ptr $ ssh_log_e
                $ case verbosity of
                    NoLog     -> ssh_LOG_NOLOG
                    Warning   -> ssh_LOG_WARNING
                    Protocol  -> ssh_LOG_PROTOCOL
                    Packet    -> ssh_LOG_PACKET
                    Functions -> ssh_LOG_FUNCTIONS
       result <- ssh_bind_options_set bind ssh_BIND_OPTIONS_LOG_VERBOSITY ptr
       free ptr
       return result

-- | Retrieve the error text message from the last error.
getError     :: Ptr Session -> IO String
getError session
  = ssh_error session >>= peekCString

-- | Retrieve the error code from the last error.
getErrorCode :: Ptr Session -> IO ErrorCode
getErrorCode session
  = do code <- ssh_error_code session
       let x | code == ssh_NO_ERROR       = NoError
             | code == ssh_REQUEST_DENIED = RequestDenied
             | code == ssh_FATAL          = Fatal
             | otherwise                  = Fatal
       return x

-- | Retrieve the error text message from the last error.
getBindError     :: Server -> IO String
getBindError bind
  = ssh_error bind >>= peekCString

-- | Retrieve the error code from the last error.
getBindErrorCode :: Server -> IO ErrorCode
getBindErrorCode bind
  = do code <- ssh_error_code bind
       let x | code == ssh_NO_ERROR       = NoError
             | code == ssh_REQUEST_DENIED = RequestDenied
             | code == ssh_FATAL          = Fatal
             | otherwise                  = Fatal
       return x