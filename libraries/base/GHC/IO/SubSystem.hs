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
  withIoSubSystem',
  whenIoSubSystem,
  IoSubSystem(..)
 ) where

import GHC.Base

import GHC.IO.Unsafe
import GHC.IO
import GHC.IORef
import GHC.RTS.Flags

import Control.Monad

ioSubSystem :: IORef IoSubSystem
ioSubSystem = unsafePerformIO sub
  where
    sub = do misc <- getMiscFlags
             newIORef (ioManager misc)

setIoSubSystem :: IoSubSystem -> IO ()
setIoSubSystem = writeIORef ioSubSystem

getIoSubSystem :: IO IoSubSystem
getIoSubSystem = readIORef ioSubSystem

withIoSubSystem :: (IoSubSystem -> IO a) -> IO a
withIoSubSystem f = do sub <- getIoSubSystem
                       f sub

withIoSubSystem' :: (IoSubSystem -> a) -> a
withIoSubSystem' f = unsafePerformIO inner
  where inner = do sub <- getIoSubSystem
                   return (f sub)

whenIoSubSystem :: IoSubSystem -> IO () -> IO ()
whenIoSubSystem m f = do sub <- getIoSubSystem
                         when (sub == m) f