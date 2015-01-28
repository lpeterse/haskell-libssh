{-# LANGUAGE ForeignFunctionInterface, CPP, EmptyDataDecls #-}

module SSH.LibSSH.Poll (
    poll_POLLIN
  , poll_POLLPRI
  , poll_POLLOUT
  , poll_POLLERR
  , poll_POLLHUP
  , poll_POLLNVAL
  ) where

import Foreign.C
import Foreign.Ptr

#include "poll.h"

poll_POLLIN :: CShort
poll_POLLIN  = #const POLLIN

poll_POLLPRI :: CShort
poll_POLLPRI = #const POLLPRI

poll_POLLOUT :: CShort
poll_POLLOUT = #const POLLOUT

poll_POLLERR :: CShort
poll_POLLERR = #const POLLERR

poll_POLLHUP :: CShort
poll_POLLHUP = #const POLLHUP

poll_POLLNVAL :: CShort
poll_POLLNVAL = #const POLLNVAL

