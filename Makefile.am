#
# Semigroups
#
# This file is part of the build system of a GAP kernel extension.
# Requires automake.
#
ACLOCAL_AMFLAGS = -I m4

SUBDIRS = src/libsemigroups
AM_CPPFLAGS = -I$(srcdir)/src/libsemigroups

BINARCHDIR = bin/$(GAPARCH)
GAPINSTALLLIB = $(abs_top_srcdir)/$(BINARCHDIR)/semigroups.so

lib_LTLIBRARIES = semigroups.la

semigroups_la_SOURCES =  src/pkg.cc src/converter.cc src/bipart.cc 
semigroups_la_SOURCES += src/uf.cc src/fropin.cc src/congpairs.cc
semigroups_la_SOURCES += src/semigrp.cc

semigroups_la_CXXFLAGS = -std=c++11 -O3 -g -march=native

semigroups_la_CPPFLAGS = $(GAP_CPPFLAGS) -DCONFIG_H
# Note that the latter is only for GAP 4.4.12
semigroups_la_LDFLAGS = -module -avoid-version

semigroups_la_LIBADD = src/libsemigroups/libsemigroups.la

if SYS_IS_CYGWIN
semigroups_la_LDFLAGS += -no-undefined -version-info 0:0:0 -Wl,$(GAPROOT)/bin/$(GAPARCH)/gap.dll
endif

all-local: semigroups.la
	$(mkdir_p) $(top_srcdir)/$(BINARCHDIR) $(top_srcdir)/bin/lib
	cp -RL src/libsemigroups/.libs/* $(top_srcdir)/bin/lib/
if SYS_IS_CYGWIN
	cp .libs/semigroups.dll $(GAPINSTALLLIB)
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
