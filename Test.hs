{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Control.Concurrent
import Control.Monad
import Control.Exception

import Data.Typeable

import Foreign.Ptr

import SSH.Server
import SSH.LibSSH.Server
import SSH.LibSSH.Session
import SSH.LibSSH.Callbacks

import Control.Concurrent.Async

main :: IO ()
main
  = do b <- ssh_bind_new

       setRsaKey "/home/lars/repos/haskell-libssh/rsa" b
       setBindPort 8005 b
       -- setLogVerbosity Warning b

       icc <- wrapIncomingConnectionCallback (\_ _-> print "Incoming connection!")
       print icc
       cbs <- ssh_bind_new_callbacks icc
       ssh_bind_set_callbacks b cbs nullPtr

       print "Start listening."
       ssh_bind_listen b   >>= print

       async $ forever 
             $ do print "Wait for incoming connection."
                  -- this session is freed on exception in 'forkSession'
                  s <- ssh_new
                  ssh_bind_accept b s >>= print
                  a <- forkSession s
                  threadDelay 10000
                  return ()

       threadDelay 10000

       forever $ do print "Main thread."
                    threadDelay 6000000

       return ()

data SessionException
   = SessionDisconnected
   | SessionSetCallbacksFailed
   | SessionKeyExchangeFailed
   deriving (Show, Typeable)

instance Exception SessionException

forkSession :: Ptr Session -> IO (Async ())
forkSession session
  = asyncBound
  $ do event <- ssh_event_new
       cb1   <- wrapAuthPasswordCallback              authPasswordCallback
       cb2   <- wrapAuthNoneCallback                  authNoneCallback
       cb3   <- wrapAuthPubkeyCallback                authPubkeyCallback
       cb4   <- wrapServiceRequestCallback            serviceRequestCallback
       cb5   <- wrapChannelOpenRequestSessionCallback channelOpenRequestSessionCallback
       cbs   <- ssh_new_server_callbacks nullPtr cb1 cb2 cb3 cb4 cb5
       (
         ( do putStrLn (show session ++ ": new session thread")

              scb <- ssh_set_server_callbacks session cbs
              when (scb /= 0) $ do
                throw SessionSetCallbacksFailed

              -- The session is not connected before key exchange succeeded.
              kex <- ssh_handle_key_exchange session
              when (kex /= 0) $ do
                throw SessionKeyExchangeFailed

              forever $ do
                -- Check whether the session is (still) connected.
                con <- ssh_is_connected session
                when (con /= 1) $ do
                  throw SessionDisconnected

                ssh_event_add_session event session
                putStrLn (show session ++ ": session thread alive")
                ssh_event_dopoll event (-1)

                -- testing
                -- ssh_disconnect session
              -- Should never the reached.
              return ()
         ) `catch` (\e->
           do print (e :: SessionException)
         ) `finally` (
           do ssh_free session
              ssh_event_free event
              ssh_free_server_callbacks cbs
              freeHaskellFunPtr cb1
              freeHaskellFunPtr cb2
              freeHaskellFunPtr cb3
              freeHaskellFunPtr cb4
              freeHaskellFunPtr cb5
         )
        )
  where
    authPasswordCallback _ _ _ _
      = do print "authPasswordCallback"
           return 0
    authNoneCallback _ _
      = do print "authNoneCallback"
           return 0
    authPubkeyCallback _ _ _ _ _
      = do print "authPubkeyCallback"
           return 0
    serviceRequestCallback _ _ _
      = do print "serviceRequestCallback"
           return 0
    channelOpenRequestSessionCallback _ _
      = do print "channelOpenRequestSessionCallback"
           return nullPtr
