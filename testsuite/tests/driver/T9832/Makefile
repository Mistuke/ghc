TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# -fforce-recomp makes lots of driver tests trivially pass, so we
# filter it out from $(TEST_HC_OPTS).
TEST_HC_OPTS_NO_RECOMP = $(filter-out -fforce-recomp,$(TEST_HC_OPTS))

# Recompilation tests

clean:
	rm -f *.o *.hi
	rm -rf *_split

T9832: clean
	'$(TEST_HC)' $(TEST_HC_OPTS_NO_RECOMP) -split-obj --make -v0 T9832hs
	
# T9832a: clean
# 	cd PowerPC
# 	tar xvf Coverage.tar.xz
# 	mkdir Coverage
# 	cp Coverage_ghc7073_0/ghc7073_2.split_s Coverage
# 	'$(GHC_SPLIT)' 'Coverage/ghc7073_2.split_s' 'Coverage/ghc7073_2.split' 'Coverage/ghc7073_2.split'
# 	tar xvf GraphOpts.tar.xz
# 	cd ..
