{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Internals
-- Copyright   :  (c) The University of Glasgow, 2017
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (requires POSIX)
--
-- hs-boot for System.Posix.Internals to expose puts which needs to break
-- the dependency with Foreign
-----------------------------------------------------------------------------

module System.Posix.Internals where

import GHC.IO
import GHC.Base

-- ---------------------------------------------------------------------------
-- Debugging the base package

puts :: String -> IO ()