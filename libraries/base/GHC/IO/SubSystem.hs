{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.IO.SubSystem
-- Copyright   :  (c) The University of Glasgow, 2017
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- The SubSystem control
--
-----------------------------------------------------------------------------

module GHC.IO.SubSystem (
  setIoSubSystem,
  getIoSubSystem,
  withIoSubSystem,
  IoSubSystem(..)
 ) where

import GHC.MVar
import GHC.IO.Unsafe
import GHC.IO

import Data.Functor
import Data.Maybe

data IoSubSystem
  = IoPOSIX   -- ^ Use a POSIX I/O Sub-System
  | IoNative  -- ^ Use platform native Sub-System. For unix OSes this is the
              --   same as IoPOSIX, but on Windows this means use the Windows
              --   native APIs for I/O, including IOCP and RIO.

defaultSubSystem :: IoSubSystem
defaultSubSystem = IoPOSIX

ioSubSystem :: MVar IoSubSystem
ioSubSystem = unsafePerformIO (newMVar IoPOSIX)

setIoSubSystem :: IoSubSystem -> IO ()
setIoSubSystem = putMVar ioSubSystem

getIoSubSystem :: IO IoSubSystem
getIoSubSystem = maybe defaultSubSystem (\x->x) `fmap` tryReadMVar ioSubSystem

withIoSubSystem :: (IoSubSystem -> IO a) -> IO a
withIoSubSystem f = do sub <- getIoSubSystem
                       f sub