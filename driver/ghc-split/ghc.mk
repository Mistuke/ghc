# -----------------------------------------------------------------------------
#
# (c) 2009 The University of Glasgow
#
# This file is part of the GHC build system.
#
# To understand how the build system works and how to modify it, see
#      http://ghc.haskell.org/trac/ghc/wiki/Building/Architecture
#      http://ghc.haskell.org/trac/ghc/wiki/Building/Modifying
#
# -----------------------------------------------------------------------------

driver/ghc-split_USES_CABAL = YES
driver/ghc-split_PACKAGE = ghc-split
driver/ghc-split_dist-install_INSTALL = YES
driver/ghc-split_dist-install_PROGNAME = ghc-split
driver/ghc-split_dist-install_INSTALL_INPLACE = YES
driver/ghc-split_dist-install_WANT_BINDIST_WRAPPER = YES

$(eval $(call build-prog,driver/ghc-split,dist-install,0))
$(eval $(call bindist,driver/ghc-split,ghc.mk))