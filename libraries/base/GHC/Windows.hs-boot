{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Windows
-- Copyright   :  (c) The University of Glasgow, 2017
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- Break the recursive imports in base. This module is used by Foreign and so
-- needs an hs-boot to make sense as this module also makes foreign calls.
--
-----------------------------------------------------------------------------

module GHC.Windows (
        -- * System errors
        throwGetLastError,
        failWith,
        getLastError
    ) where

import GHC.Base
import GHC.IO
import GHC.Windows.Types

failWith :: String -> ErrCode -> IO a
getLastError :: IO ErrCode
throwGetLastError :: String -> IO a
