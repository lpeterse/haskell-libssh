{-# LANGUAGE ForeignFunctionInterface, CPP, EmptyDataDecls #-}

module SSH.LibSSH.Server (
    version

  , Bind
  , Session
  , Message
  , UserData

  -- * Enum values
  -- ** Bind Options
  , SSH_BIND_OPTIONS_E
  , ssh_BIND_OPTIONS_BINDADDR
  , ssh_BIND_OPTIONS_BINDPORT
  , ssh_BIND_OPTIONS_BINDPORT_STR
  , ssh_BIND_OPTIONS_HOSTKEY
  , ssh_BIND_OPTIONS_DSAKEY
  , ssh_BIND_OPTIONS_RSAKEY
  , ssh_BIND_OPTIONS_BANNER
  , ssh_BIND_OPTIONS_LOG_VERBOSITY

  , SSH_OPTIONS_E
  , ssh_OPTIONS_HOST                       
  , ssh_OPTIONS_PORT                       
  , ssh_OPTIONS_PORT_STR                   
  , ssh_OPTIONS_FD                         
  , ssh_OPTIONS_USER                       
  , ssh_OPTIONS_SSH_DIR                    
  , ssh_OPTIONS_IDENTITY                   
  , ssh_OPTIONS_ADD_IDENTITY               
  , ssh_OPTIONS_KNOWNHOSTS                 
  , ssh_OPTIONS_TIMEOUT                    
  , ssh_OPTIONS_TIMEOUT_USEC               
  , ssh_OPTIONS_SSH1                       
  , ssh_OPTIONS_SSH2                       
  , ssh_OPTIONS_LOG_VERBOSITY              
  , ssh_OPTIONS_LOG_VERBOSITY_STR          
  , ssh_OPTIONS_CIPHERS_C_S                
  , ssh_OPTIONS_CIPHERS_S_C                
  , ssh_OPTIONS_COMPRESSION_C_S            
  , ssh_OPTIONS_COMPRESSION_S_C            
  , ssh_OPTIONS_PROXYCOMMAND               
  , ssh_OPTIONS_BINDADDR                   
  , ssh_OPTIONS_STRICTHOSTKEYCHECK         
  , ssh_OPTIONS_COMPRESSION                
  , ssh_OPTIONS_COMPRESSION_LEVEL          
  , ssh_OPTIONS_KEY_EXCHANGE               
  , ssh_OPTIONS_HOSTKEYS                   
  , ssh_OPTIONS_GSSAPI_SERVER_IDENTITY     
  , ssh_OPTIONS_GSSAPI_CLIENT_IDENTITY     
  , ssh_OPTIONS_GSSAPI_DELEGATE_CREDENTIALS

  -- ** Log
  , SSH_LOG_E (..)
  , ssh_LOG_NOLOG
  , ssh_LOG_WARNING
  , ssh_LOG_PROTOCOL
  , ssh_LOG_PACKET
  , ssh_LOG_FUNCTIONS

  -- ** Request
  , SSH_REQUEST_E (..)
  , ssh_REQUEST_AUTH
  , ssh_REQUEST_CHANNEL_OPEN
  , ssh_REQUEST_CHANNEL
  , ssh_REQUEST_SERVICE
  , ssh_REQUEST_GLOBAL

  , ssh_AUTH_METHOD_UNKNOWN
  , ssh_AUTH_METHOD_NONE
  , ssh_AUTH_METHOD_PASSWORD
  , ssh_AUTH_METHOD_PUBLICKEY
  , ssh_AUTH_METHOD_HOSTBASED
  , ssh_AUTH_METHOD_INTERACTIVE

  , ssh_NO_ERROR
  , ssh_REQUEST_DENIED
  , ssh_FATAL

  , ssh_error
  , ssh_error_code

  , ssh_bind_new
  , ssh_bind_free
  , ssh_bind_options_set
  , ssh_bind_listen
  , ssh_bind_accept
  , ssh_bind_set_callbacks
  , ssh_bind_new_callbacks
  , ssh_bind_free_callbacks

  , wrapIncomingConnectionCallback

  , ssh_init
  , ssh_new
  , ssh_free

  , ssh_handle_key_exchange
  , ssh_message_get
  , ssh_message_type
  , ssh_message_subtype
  , ssh_message_free

  , ssh_get_pubkey_hash

  , ssh_options_set

  ) where

import Foreign.C
import Foreign.Ptr

#include "libssh/libssh.h"
#include "libssh/server.h"

version :: String
version  = concat
             [ show ((#const LIBSSH_VERSION_MAJOR) :: Int)
             , "."
             , show ((#const LIBSSH_VERSION_MINOR) :: Int)
             , "."
             , show ((#const LIBSSH_VERSION_MICRO) :: Int)
             ]

data Bind
data BindCallbacks
data UserData
data Session
data Message

newtype SSH_BIND_OPTIONS_E = SSH_BIND_OPTIONS_E CInt
#{enum SSH_BIND_OPTIONS_E, SSH_BIND_OPTIONS_E,
    ssh_BIND_OPTIONS_BINDADDR = SSH_BIND_OPTIONS_BINDADDR,
    ssh_BIND_OPTIONS_BINDPORT  = SSH_BIND_OPTIONS_BINDPORT,
    ssh_BIND_OPTIONS_BINDPORT_STR  = SSH_BIND_OPTIONS_BINDPORT_STR,
    ssh_BIND_OPTIONS_HOSTKEY  = SSH_BIND_OPTIONS_HOSTKEY,
    ssh_BIND_OPTIONS_DSAKEY  = SSH_BIND_OPTIONS_DSAKEY,
    ssh_BIND_OPTIONS_RSAKEY  = SSH_BIND_OPTIONS_RSAKEY,
    ssh_BIND_OPTIONS_BANNER  = SSH_BIND_OPTIONS_BANNER,
    ssh_BIND_OPTIONS_LOG_VERBOSITY  = SSH_BIND_OPTIONS_LOG_VERBOSITY
}

newtype SSH_OPTIONS_E = SSH_OPTIONS_E CInt
#{enum SSH_OPTIONS_E, SSH_OPTIONS_E,
    ssh_OPTIONS_HOST                        = SSH_OPTIONS_HOST,
    ssh_OPTIONS_PORT                        = SSH_OPTIONS_PORT,
    ssh_OPTIONS_PORT_STR                    = SSH_OPTIONS_PORT_STR,
    ssh_OPTIONS_FD                          = SSH_OPTIONS_FD,
    ssh_OPTIONS_USER                        = SSH_OPTIONS_USER,
    ssh_OPTIONS_SSH_DIR                     = SSH_OPTIONS_SSH_DIR,
    ssh_OPTIONS_IDENTITY                    = SSH_OPTIONS_IDENTITY,
    ssh_OPTIONS_ADD_IDENTITY                = SSH_OPTIONS_ADD_IDENTITY,
    ssh_OPTIONS_KNOWNHOSTS                  = SSH_OPTIONS_KNOWNHOSTS,
    ssh_OPTIONS_TIMEOUT                     = SSH_OPTIONS_TIMEOUT,
    ssh_OPTIONS_TIMEOUT_USEC                = SSH_OPTIONS_TIMEOUT_USEC,
    ssh_OPTIONS_SSH1                        = SSH_OPTIONS_SSH1,
    ssh_OPTIONS_SSH2                        = SSH_OPTIONS_SSH2,
    ssh_OPTIONS_LOG_VERBOSITY               = SSH_OPTIONS_LOG_VERBOSITY,
    ssh_OPTIONS_LOG_VERBOSITY_STR           = SSH_OPTIONS_LOG_VERBOSITY_STR,
    ssh_OPTIONS_CIPHERS_C_S                 = SSH_OPTIONS_CIPHERS_C_S,
    ssh_OPTIONS_CIPHERS_S_C                 = SSH_OPTIONS_CIPHERS_S_C,
    ssh_OPTIONS_COMPRESSION_C_S             = SSH_OPTIONS_COMPRESSION_C_S,
    ssh_OPTIONS_COMPRESSION_S_C             = SSH_OPTIONS_COMPRESSION_S_C,
    ssh_OPTIONS_PROXYCOMMAND                = SSH_OPTIONS_PROXYCOMMAND,
    ssh_OPTIONS_BINDADDR                    = SSH_OPTIONS_BINDADDR,
    ssh_OPTIONS_STRICTHOSTKEYCHECK          = SSH_OPTIONS_STRICTHOSTKEYCHECK,
    ssh_OPTIONS_COMPRESSION                 = SSH_OPTIONS_COMPRESSION,
    ssh_OPTIONS_COMPRESSION_LEVEL           = SSH_OPTIONS_COMPRESSION_LEVEL,
    ssh_OPTIONS_KEY_EXCHANGE                = SSH_OPTIONS_KEY_EXCHANGE,
    ssh_OPTIONS_HOSTKEYS                    = SSH_OPTIONS_HOSTKEYS,
    ssh_OPTIONS_GSSAPI_SERVER_IDENTITY      = SSH_OPTIONS_GSSAPI_SERVER_IDENTITY,
    ssh_OPTIONS_GSSAPI_CLIENT_IDENTITY      = SSH_OPTIONS_GSSAPI_CLIENT_IDENTITY,
    ssh_OPTIONS_GSSAPI_DELEGATE_CREDENTIALS = SSH_OPTIONS_GSSAPI_DELEGATE_CREDENTIALS
}

newtype SSH_LOG_E = SSH_LOG_E { ssh_log_e :: CInt }
#{enum SSH_LOG_E, SSH_LOG_E,
    ssh_LOG_NOLOG = SSH_LOG_NOLOG,
    ssh_LOG_WARNING = SSH_LOG_WARNING,
    ssh_LOG_PROTOCOL = SSH_LOG_PROTOCOL,
    ssh_LOG_PACKET = SSH_LOG_PACKET,
    ssh_LOG_FUNCTIONS = SSH_LOG_FUNCTIONS
}

newtype SSH_REQUEST_E = SSH_REQUEST_E { ssh_request_e :: CInt } deriving (Eq, Show)
#{enum SSH_REQUEST_E, SSH_REQUEST_E,
  ssh_REQUEST_AUTH = SSH_REQUEST_AUTH,
  ssh_REQUEST_CHANNEL_OPEN = SSH_REQUEST_CHANNEL_OPEN,
  ssh_REQUEST_CHANNEL = SSH_REQUEST_CHANNEL,
  ssh_REQUEST_SERVICE = SSH_REQUEST_SERVICE,
  ssh_REQUEST_GLOBAL = SSH_REQUEST_GLOBAL
};

ssh_AUTH_METHOD_UNKNOWN     :: CInt
ssh_AUTH_METHOD_UNKNOWN      = #const SSH_AUTH_METHOD_UNKNOWN
ssh_AUTH_METHOD_NONE        :: CInt
ssh_AUTH_METHOD_NONE         = #const SSH_AUTH_METHOD_NONE
ssh_AUTH_METHOD_PASSWORD    :: CInt
ssh_AUTH_METHOD_PASSWORD     = #const SSH_AUTH_METHOD_PASSWORD
ssh_AUTH_METHOD_PUBLICKEY   :: CInt
ssh_AUTH_METHOD_PUBLICKEY    = #const SSH_AUTH_METHOD_PUBLICKEY
ssh_AUTH_METHOD_HOSTBASED   :: CInt
ssh_AUTH_METHOD_HOSTBASED    = #const SSH_AUTH_METHOD_HOSTBASED
ssh_AUTH_METHOD_INTERACTIVE :: CInt
ssh_AUTH_METHOD_INTERACTIVE  = #const SSH_AUTH_METHOD_INTERACTIVE

ssh_NO_ERROR       :: CInt
ssh_NO_ERROR        = #const SSH_NO_ERROR
ssh_REQUEST_DENIED :: CInt
ssh_REQUEST_DENIED  = #const SSH_REQUEST_DENIED
ssh_FATAL          :: CInt
ssh_FATAL           = #const SSH_FATAL

foreign import ccall safe "libssh/libssh.h ssh_init"
  ssh_init :: IO CInt

foreign import ccall safe "libssh/server.h ssh_get_error"
  ssh_error :: Ptr a -> IO CString

foreign import ccall safe "libssh/server.h ssh_get_error_code"
  ssh_error_code :: Ptr a -> IO CInt

foreign import ccall safe "libssh/server.h ssh_bind_new"
  ssh_bind_new :: IO (Ptr Bind)

foreign import ccall safe "libssh/server.h ssh_bind_free"
  ssh_bind_free :: Ptr Bind -> IO ()

foreign import ccall safe "libssh/server.h ssh_bind_set_callbacks"
  ssh_bind_set_callbacks :: Ptr Bind -> Ptr BindCallbacks -> Ptr UserData -> IO ()

type IncomingConnectionCallback = Ptr Bind -> Ptr UserData -> IO ()

foreign import ccall safe "misc.h ssh_bind_new_callbacks"
  ssh_bind_new_callbacks :: FunPtr IncomingConnectionCallback -> IO (Ptr BindCallbacks)

foreign import ccall "wrapper"
  wrapIncomingConnectionCallback :: IncomingConnectionCallback -> IO (FunPtr IncomingConnectionCallback)

foreign import ccall safe "misc.h ssh_bind_free_callbacks"
  ssh_bind_free_callbacks :: Ptr BindCallbacks -> IO ()

foreign import ccall safe "libssh/server.h ssh_bind_options_set"
  ssh_bind_options_set :: Ptr Bind -> SSH_BIND_OPTIONS_E -> Ptr a -> IO CInt

foreign import ccall safe "libssh/server.h ssh_bind_listen"
  ssh_bind_listen :: Ptr Bind -> IO CInt

foreign import ccall safe "libssh/server.h ssh_bind_accept"
  ssh_bind_accept :: Ptr Bind -> Ptr Session -> IO CInt

foreign import ccall safe "libssh/libssh.h ssh_get_pubkey_hash"
  ssh_get_pubkey_hash :: Ptr Session -> Ptr (Ptr CChar) -> IO CInt

foreign import ccall safe "libssh/server.h ssh_handle_key_exchange"
  ssh_handle_key_exchange :: Ptr Session -> IO CInt

-- | Retrieve a SSH message from a SSH session.
--
-- The SSH message received, NULL in case of error, or timeout elapsed.
-- This function blocks until a message has been received. 
-- Better set up a callback if this behavior is unwanted.
foreign import ccall safe "libssh/libssh.h ssh_message_get"
  ssh_message_get :: Ptr Session -> IO (Ptr Message)

foreign import ccall safe "libssh/libssh.h ssh_message_free"
  ssh_message_free :: Ptr Message -> IO ()

foreign import ccall safe "libssh/libssh.h ssh_message_type"
  ssh_message_type :: Ptr Message -> IO SSH_REQUEST_E

foreign import ccall safe "libssh/libssh.h ssh_message_subtype"
  ssh_message_subtype :: Ptr Message -> IO CInt

foreign import ccall safe "libssh/libssh.h ssh_new"
  ssh_new :: IO (Ptr Session)

foreign import ccall safe "libssh/libssh.h ssh_free"
  ssh_free :: Ptr Session -> IO ()

foreign import ccall safe "libssh/server.h ssh_options_set"
  ssh_options_set :: Ptr Session -> SSH_OPTIONS_E -> Ptr a -> IO CInt
