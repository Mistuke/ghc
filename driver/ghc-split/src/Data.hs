----------------------------------------------------------------------------- 
-- 
-- Assembly file splitter, data definitions
-- 
-- (c) The GHC Team 2015 
-- 
----------------------------------------------------------------------------- 
module Data where

import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M

-- | Target OSes as defined in aclocal.m4 under checkOS()
data TargetOS
  = Linux       -- ^ linux
  | IOS         -- ^ ios
  | Darwin      -- ^ darwin
  | Solaris2    -- ^ solaris2
  | MingW32     -- ^ mingw32
  | FreeBSD     -- ^ freebsd
  | DragonFly   -- ^ dragonfly
  | KFreeBSDGnu -- ^ kfreebsdgnu
  | OpenBSD     -- ^ openbsd
  | NetBSD      -- ^ netbsd
  | Haiku       -- ^ haiku
  | OSF3        -- ^ osf3
  | NtoGNX      -- ^ nto-gnx
  | Android     -- ^ linux-android
  | UnknownOS   -- ^ The rest
    deriving (Show, Eq)
    
-- | Target Architectures as defined in aclocal.m4 under checkArch()
data TargetArch
  = X86         -- ^ i386
  | X86_64      -- ^ x64_64
  | PowerPC     -- ^ powerpc
  | PowerPC64   -- ^ powerpc64
  | Sparc       -- ^ sparc
  | Arm         -- ^ arm
  | Arm64       -- ^ aarch64
  | Alpha       -- ^ alpha
  | Mips        -- ^ mips|mipseb
  | Mipsel      -- ^ mipsel
  | Hppa        -- ^ hppa
  | UnknownArch -- ^ The rest
    deriving (Show, Eq)
    
-- | Target vendor as defined in aclocal.m4 under checkVendor()
data TargetVendor
  = Dec        -- ^ dec
  | Unknown    -- ^ unknown
  | HP         -- ^ hp
  | Apple      -- ^ apple
  | Next       -- ^ next
  | Sun        -- ^ sun
  | SGI        -- ^ sgi
  | IBM        -- ^ ibm
  | MontaVista -- ^ montavista
  | PortBld    -- ^ portbld
    deriving (Show, Eq)
    
type LocalConstants = M.Map B.ByteString B.ByteString
type LocalExports   = M.Map B.ByteString B.ByteString