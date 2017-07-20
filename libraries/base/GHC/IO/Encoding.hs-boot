{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GHC.IO.Encoding where

import GHC.IO (IO)
import GHC.IO.Buffer
import GHC.IO.Encoding.Types

getLocaleEncoding, getFileSystemEncoding, getForeignEncoding :: Encodable e => IO (TextEncoding e)

