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

import GHC.IO.Unsafe
import GHC.IO
import GHC.IORef
import GHC.RTS.Flags

import Data.Functor
import Data.Maybe

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