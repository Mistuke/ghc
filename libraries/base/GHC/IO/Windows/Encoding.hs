{-# LANGUAGE CPP #-}
{- |
   Module      :  System.Win32.Encoding
   Copyright   :  2012 shelarcy
   License     :  BSD-style

   Maintainer  :  shelarcy@gmail.com
   Stability   :  Provisional
   Portability :  Non-portable (Win32 API)

   Enocode/Decode mutibyte charactor using Win32 API.
-}

module GHC.IO.Windows.Encoding
  ( encodeMultiByte
  , encodeMultiByteIO
  , encodeMultiByteRawIO
  , decodeMultiByte
  , decodeMultiByteIO
  , wideCharToMultiByte
  , multiByteToWideChar
  ) where

import Foreign.C.Types        (CInt(..))
import Foreign.C.String       (peekCAStringLen, peekCWStringLen,
                               withCWStringLen, withCAStringLen, )
import Foreign.Ptr (nullPtr)
import Foreign.Marshal.Array  (allocaArray)
import Foreign.Marshal.Unsafe (unsafeLocalState)
import GHC.Windows
import GHC.IO.Encoding.CodePage (CodePage)

#include "windows_cconv.h"

-- | The "System.IO" output functions (e.g. `putStr`) don't
-- automatically convert to multibyte string on Windows, so this
-- function is provided to make the conversion from a Unicode string
-- in the given code page to a proper multibyte string.  To get the
-- code page for the console, use `getCurrentCodePage`.
--
encodeMultiByte :: CodePage -> String -> String
encodeMultiByte cp = unsafeLocalState . encodeMultiByteIO cp

encodeMultiByteIO :: CodePage -> String -> IO String
encodeMultiByteIO _ "" = return ""
  -- WideCharToMultiByte doesn't handle empty strings
encodeMultiByteIO cp wstr =
  withCWStringLen wstr $ \(cwstr,len) -> do
    mbchars' <- failIfZero "WideCharToMultiByte" $ wideCharToMultiByte
                cp
                0
                cwstr
                (fromIntegral len)
                nullPtr 0
                nullPtr nullPtr
    -- mbchar' is the length of buffer required
    allocaArray (fromIntegral mbchars') $ \mbstr -> do
      mbchars <- failIfZero "WideCharToMultiByte" $ wideCharToMultiByte
                 cp
                 0
                 cwstr
                 (fromIntegral len)
                 mbstr mbchars'
                 nullPtr nullPtr
      peekCAStringLen (mbstr,fromIntegral mbchars)  -- converts [Char] to UTF-16

encodeMultiByteRawIO :: CodePage -> String -> IO (LPCSTR, CInt)
encodeMultiByteRawIO _ "" = return (nullPtr, 0)
  -- WideCharToMultiByte doesn't handle empty strings
encodeMultiByteRawIO cp wstr =
  withCWStringLen wstr $ \(cwstr,len) -> do
    mbchars' <- failIfZero "WideCharToMultiByte" $ wideCharToMultiByte
                cp
                0
                cwstr
                (fromIntegral len)
                nullPtr 0
                nullPtr nullPtr
    -- mbchar' is the length of buffer required
    allocaArray (fromIntegral mbchars') $ \mbstr -> do
      mbchars <- failIfZero "WideCharToMultiByte" $ wideCharToMultiByte
                 cp
                 0
                 cwstr
                 (fromIntegral len)
                 mbstr mbchars'
                 nullPtr nullPtr
      return (mbstr,fromIntegral mbchars)  -- converts [Char] to UTF-16

foreign import WINDOWS_CCONV "WideCharToMultiByte"
  wideCharToMultiByte
        :: CodePage
        -> DWORD   -- dwFlags,
        -> LPCWSTR -- lpWideCharStr
        -> CInt    -- cchWideChar
        -> LPSTR   -- lpMultiByteStr
        -> CInt    -- cbMultiByte
        -> LPCSTR  -- lpMultiByteStr
        -> LPBOOL  -- lpbFlags
        -> IO CInt

-- | The `System.IO` input functions (e.g. `getLine`) don't
-- automatically convert to Unicode, so this function is provided to
-- make the conversion from a multibyte string in the given code page
-- to a proper Unicode string.  To get the code page for the console,
-- use `getConsoleCP`.
stringToUnicode :: CodePage -> String -> IO String
stringToUnicode _cp "" = return ""
     -- MultiByteToWideChar doesn't handle empty strings (#1929)
stringToUnicode cp mbstr =
  withCAStringLen mbstr $ \(cstr,len) -> do
    wchars <- failIfZero "MultiByteToWideChar" $ multiByteToWideChar
                cp
                0
                cstr
                (fromIntegral len)
                nullPtr 0
    -- wchars is the length of buffer required
    allocaArray (fromIntegral wchars) $ \cwstr -> do
      wchars' <- failIfZero "MultiByteToWideChar" $ multiByteToWideChar
                cp
                0
                cstr
                (fromIntegral len)
                cwstr wchars
      peekCWStringLen (cwstr,fromIntegral wchars')  -- converts UTF-16 to [Char]

foreign import WINDOWS_CCONV unsafe "MultiByteToWideChar"
  multiByteToWideChar
        :: CodePage
        -> DWORD   -- dwFlags,
        -> LPCSTR  -- lpMultiByteStr
        -> CInt    -- cbMultiByte
        -> LPWSTR  -- lpWideCharStr
        -> CInt    -- cchWideChar
        -> IO CInt

decodeMultiByte :: CodePage -> String -> String
decodeMultiByte cp = unsafeLocalState . decodeMultiByteIO cp

-- | Because of `stringToUnicode` is unclear name, we use `decodeMultiByteIO`
-- for alias of `stringToUnicode`.
decodeMultiByteIO :: CodePage -> String -> IO String
decodeMultiByteIO = stringToUnicode
{-# INLINE decodeMultiByteIO #-}
