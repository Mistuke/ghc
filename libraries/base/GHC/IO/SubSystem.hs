{-# LANGUAGE Trustworthy       #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP               #-}

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
  getCharBufEncoding,
  IoSubSystem(..),
  CharBufEncoding(..)
 ) where

import GHC.IO.Unsafe
import GHC.IO
import GHC.IORef
import GHC.RTS.Flags

import Control.Monad
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

withIoSubSystem' :: (IoSubSystem -> a) -> a
withIoSubSystem' f = unsafePerformIO inner
  where inner = do sub <- getIoSubSystem
                   return (f sub)

data CharBufEncoding = CharBuff_UTF16 | CharBuff_UTF32

getCharBufEncoding :: CharBufEncoding
getCharBufEncoding = withIoSubSystem' inner
  where inner IoPOSIX = CharBuff_UTF32
#if defined(mingw32_HOST_OS)
        inner IoNative = CharBuff_UTF16
#else
        inner IoNative = CharBuff_UTF32
#endif