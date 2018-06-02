{-# LANGUAGE MagicHash, NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Conc.Sync [boot]
-- Copyright   :  (c) The University of Glasgow, 1994-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Basic concurrency stuff.
--
-----------------------------------------------------------------------------

module GHC.Conc.Sync
        ( forkIO,
          TVar(..),
          ThreadId(..),
          myThreadId,
          showThreadId
        ) where

import GHC.Base
import GHC.Show

forkIO :: IO () -> IO ThreadId

data ThreadId = ThreadId ThreadId#
data TVar a = TVar (TVar# RealWorld a)

myThreadId :: IO ThreadId
showThreadId :: ThreadId -> String