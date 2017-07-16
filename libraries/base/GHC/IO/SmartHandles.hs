{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.IO.SmartHandles
-- Copyright   :  (c) The University of Glasgow, 2017
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- This model abtracts away the platform specific handles that can be toggled
-- through the RTS.
--
-----------------------------------------------------------------------------

module GHC.IO.SmartHandles
  ( -- std handles
    stdin, stdout, stderr,
    openFile, openBinaryFile
  ) where

import GHC.IO
import GHC.IO.IOMode
import GHC.IO.SubSystem
import GHC.IO.Handle.Types

import qualified GHC.IO.Handle.FD as POSIX
#if defined(mingw32_HOST_OS)
import qualified GHC.IO.Handle.Windows as Win
#endif

stdin :: Handle
stdin =  withIoSubSystem' sub
  where
    sub = \s -> case s of
                  IoPOSIX -> POSIX.stdin
#if defined(mingw32_HOST_OS)
                  IoNative -> Win.stdin
#else
                  IoNative -> POSIX.stdin
#endif

stdout :: Handle
stdout =  withIoSubSystem' sub
  where
    sub = \s -> case s of
                  IoPOSIX -> POSIX.stdout
#if defined(mingw32_HOST_OS)
                  IoNative -> Win.stdout
#else
                  IoNative -> POSIX.stdout
#endif

stderr :: Handle
stderr =  withIoSubSystem' sub
  where
    sub = \s -> case s of
                  IoPOSIX -> POSIX.stderr
#if defined(mingw32_HOST_OS)
                  IoNative -> Win.stderr
#else
                  IoNative -> POSIX.stderr
#endif

openFile :: FilePath -> IOMode -> IO Handle
openFile = POSIX.openFile

openBinaryFile :: FilePath -> IOMode -> IO Handle
openBinaryFile = POSIX.openBinaryFile