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
   | SessionKeyExchangeFailed
   deriving (Show, Typeable)

instance Exception SessionException

forkSession :: Ptr Session -> IO (Async ())
forkSession session
  = asyncBound
  $ do event <- ssh_event_new
       (
         ( do putStrLn (show session ++ ": new session thread")

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
                ssh_event_dopoll event 10000

                -- testing
                -- ssh_disconnect session
              -- Should never the reached.
              return ()
         ) `catch` (\e->
           do print (e :: SessionException)
         ) `finally` (
           do ssh_free session
              ssh_event_free event
         )
        )
