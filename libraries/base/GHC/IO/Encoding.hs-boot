{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GHC.IO.Encoding where

import GHC.IO (IO)
import GHC.IO.Buffer
import GHC.IO.Encoding.Types
import qualified GHC.IO.Encoding.CodePage.API as API

class API.CpEncoding e => CharEncoding e where
    getLocaleEncoding     :: IO (TextEncoding e)
    getFileSystemEncoding :: IO (TextEncoding e)
    getForeignEncoding    :: IO (TextEncoding e)
    setLocaleEncoding     :: TextEncoding e -> IO ()
    setFileSystemEncoding :: TextEncoding e -> IO ()
    setForeignEncoding    :: TextEncoding e -> IO ()

instance CharEncoding Word16 where
instance CharEncoding Char where