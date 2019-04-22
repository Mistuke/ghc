# -----------------------------------------------------------------------------
#
# (c) 2009 The University of Glasgow
#
# This file is part of the GHC build system.
#
# To understand how the build system works and how to modify it, see
#      https://gitlab.haskell.org/ghc/ghc/wikis/building/architecture
#      https://gitlab.haskell.org/ghc/ghc/wikis/building/modifying
#
# -----------------------------------------------------------------------------


define hs-sources # args: $1 = dir, $2 = distdir

ifeq "$$($1_$2_HS_SRC_DIRS)" ""
$1_$2_HS_SRC_DIRS = .
endif

# Here we collect all the .hs/.lhs source files that we can find.  If
# we can't find a Haskell source file for a given module, then presumably
# it can be generated by preprocessing something (.hsc, .y, .x etc.), so
# we depend on dist/build/Foo.hs in anticipation that the implicit rules
# will put the preprocessed source file there.
#
# NB. use :=, we only want this thing evaluated once.
#
$1_$2_HS_SRCS := $$(foreach file,$$($1_$2_SLASH_MODS),\
                 $$(firstword \
                   $$(wildcard \
                     $$(foreach dir,$$($1_$2_HS_SRC_DIRS) $2/build/$$(or $$($1_EXECUTABLE),$$($1_$2_PROGNAME),.)/autogen,\
                        $1/$$(dir)/$$(file).hs $1/$$(dir)/$$(file).lhs)) \
                   $1/$2/build/$$(file).hs))

# .hs-boot files must be in the same place as the .hs file they go
# with (GHC assumes this).  When we preprocess a source file, and
# that module has a .hs-boot or .lhs-boot file, we must arrange to
# copy the file into the distdir so that it ends up alongside the
# preprocessed .hs file.  This complicated macro figures out for which
# files we need to do this, so we can add them as dependencies of the
# .depend file rule.
#
# for each .hs file in the build dir,
# if there is a .hs-boot or .lhs-boot file for it in a source dir,
# we want that file in the build dir.
#
# NB. use :=, we only want this thing evaluated once.
#
$1_$2_HS_BOOT_SRCS := $$(foreach dir,$$($1_$2_HS_SRC_DIRS),\
                       $$(subst $1/$$(dir),$1/$2/build,\
                        $$(wildcard \
                         $$(subst $1/$2/build,$1/$$(dir),\
                          $$(foreach file,\
                           $$(filter $1/$2/build%,$$($1_$2_HS_SRCS)),\
                           $$(patsubst %.hs,%.hs-boot,$$(file)) \
                           $$(patsubst %.hs,%.lhs-boot,$$(file)))))))

endef
