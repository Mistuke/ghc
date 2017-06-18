{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (concurrency)
--
-- A common interface to a collection of useful concurrency
-- abstractions.
--
-----------------------------------------------------------------------------
module Control.Concurrent (
        -- * Bound Threads
        rtsSupportsBoundThreads,
    ) where

import Data.Bool

rtsSupportsBoundThreads :: Bool