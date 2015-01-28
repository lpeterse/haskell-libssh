module Main where

import Control.Concurrent
import Control.Monad

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
                  s <- ssh_new
                  ssh_bind_accept b s >>= print
                  a <- forkSession s
                  threadDelay 10000
                  return ()

       threadDelay 10000

       forever $ do print "Main thread."
                    threadDelay 6000000

       return ()

forkSession :: Ptr Session -> IO (Async ())
forkSession session
  = asyncBound
  $ do putStrLn (show session ++ ": new session thread")
       kex <- ssh_handle_key_exchange session
       putStrLn (show session ++ ": kex " ++ show kex)
       forever $ do c <- ssh_is_connected session
                    print "connection"
                    print c
                    case c of
                      1 -> do putStrLn (show session ++ ": session thread alive")
                      0 -> do putStrLn (show session ++ ": session dead")
                    threadDelay 5000000
                    ssh_disconnect session
