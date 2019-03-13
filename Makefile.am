#
# Semigroups
#
# This file is part of the build system of a GAP kernel extension.
# Requires automake.
#
ACLOCAL_AMFLAGS = -I m4

if WITH_INCLUDED_LIBSEMIGROUPS
  SUBDIRS = libsemigroups
  # the following triggers "make install" on libsemigroups
  # note that making it a concrete file prevents this target
  # to be needlessly re-run if libsemigroups/ is not changed.
  BUILT_SOURCES = bin/include/libsemigroups/blocks.h
endif

AM_CPPFLAGS = @LIBSEMIGROUPS_CFLAGS@

BINARCHDIR = bin/$(GAPARCH)
GAPINSTALLLIB = $(abs_top_srcdir)/$(BINARCHDIR)/semigroups.so

lib_LTLIBRARIES = semigroups.la

semigroups_la_SOURCES =  src/pkg.cc src/converter.cc src/bipart.cc 
semigroups_la_SOURCES += src/uf.cc src/fropin.cc src/congpairs.cc
semigroups_la_SOURCES += src/semigrp.cc

semigroups_la_CXXFLAGS = $(GAP_CFLAGS) @LIBSEMIGROUPS_CFLAGS@ -std=gnu++11 -O3 -g -march=native
semigroups_la_CPPFLAGS = $(GAP_CPPFLAGS)  @LIBSEMIGROUPS_CFLAGS@
semigroups_la_CFLAGS = $(GAP_CFLAGS)
semigroups_la_LDFLAGS = $(GAP_LDFLAGS) -module -avoid-version

semigroups_la_LIBADD = @LIBSEMIGROUPS_LIBS@

if SYS_IS_CYGWIN
semigroups_la_LDFLAGS += -no-undefined -version-info 0:0:0 -Wl,$(GAPROOT)/bin/$(GAPARCH)/gap.dll
endif

# the following is only run if BUILT_SOURCES is wound up
bin/include/libsemigroups/blocks.h:
	$(MAKE) -C libsemigroups install

all-local: semigroups.la
	$(mkdir_p) $(top_srcdir)/$(BINARCHDIR)
if SYS_IS_CYGWIN
	cp .libs/semigroups.dll $(GAPINSTALLLIB)
if WITH_INCLUDED_LIBSEMIGROUPS
# Cygwin will only look in this directory for dlls
	cp libsemigroups/.libs/cygsemigroups-0.dll $(GAPROOT)/.libs
endif
else
	cp .libs/semigroups.so $(GAPINSTALLLIB)
endif
	
if KERNEL_DEBUG
	@echo "SUCCESS (debug mode enabled)!"
else
	@echo "SUCCESS!"
endif

distclean-local:
	rm -rf bin/*

doc: doc/manual.six

doc/manual.six: doc/*.xml PackageInfo.g
	($(GAPROOT)/bin/gap.sh -A makedoc.g)

gaplint: 
	gaplint *.g gap/options.g gap/*.gi gap/*.gd gap/*/* doc/*.xml tst/*.tst tst/*/*.tst

cpplint:
	cpplint src/*.h src/*.cc

lint: gaplint cpplint

format: 
	clang-format -i src/*.h src/*.cc

superclean: distclean
	git clean -xdf --exclude *.swp --exclude *.swo
.PHONY: superclean
