{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Windows.Types
-- Copyright   :  (c) The University of Glasgow, 2017
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- Windows basic types for use with API calls.
--
-- ToDo: this just duplicates part of System.Win32.Types, which isn't
-- available yet.  We should move some Win32 functionality down here,
-- maybe as part of the grand reorganisation of the base package...
--
-----------------------------------------------------------------------------

module GHC.Windows.Types (
    BOOL,
    LPBOOL,
    BYTE,
    DWORD,
    DDWORD,
    UINT,
    ErrCode,
    HANDLE,
    LPWSTR,
    LPTSTR,
    LPCTSTR,
    LPVOID,
    LPDWORD,
    LPSTR,
    LPCSTR,
    LPCWSTR
    ) where

import Data.Bool
import Data.Word
import Foreign.C.Types
import Foreign.Ptr

type BOOL    = Bool
type LPBOOL  = Ptr BOOL
type BYTE    = Word8
type DWORD   = Word32
type UINT    = Word32
type ErrCode = DWORD
type HANDLE  = Ptr ()
type LPWSTR  = Ptr CWchar
type LPCTSTR = LPTSTR
type LPVOID  = Ptr ()
type LPDWORD = Ptr DWORD
type LPSTR   = Ptr CChar
type LPCSTR  = LPSTR
type LPCWSTR = LPWSTR

-- Not really a basic type, but used in many places
type DDWORD        = Word64

-- | Be careful with this.  LPTSTR can mean either WCHAR* or CHAR*, depending
-- on whether the UNICODE macro is defined in the corresponding C code.
-- Consider using LPWSTR instead.
type LPTSTR = LPWSTR