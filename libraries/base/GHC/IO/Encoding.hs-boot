{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GHC.IO.Encoding where

import Data.Word

import GHC.Base
import GHC.IO (IO)
import GHC.IO.Buffer
import GHC.IO.Encoding.Types
import qualified GHC.IO.Encoding.CodePage.API as API

class API.CpEncoding e => CharEncoding e where
    getLocaleEncoding     :: IO (TextEncoding e)
    getLocaleEncoding = undefined
    getFileSystemEncoding :: IO (TextEncoding e)
    getFileSystemEncoding = undefined
    getForeignEncoding    :: IO (TextEncoding e)
    getForeignEncoding = undefined
    setLocaleEncoding     :: TextEncoding e -> IO ()
    setLocaleEncoding = undefined
    setFileSystemEncoding :: TextEncoding e -> IO ()
    setFileSystemEncoding = undefined
    setForeignEncoding    :: TextEncoding e -> IO ()
    setForeignEncoding = undefined

instance CharEncoding Word16 where
instance CharEncoding Char where