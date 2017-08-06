#
# Semigroups
#
# This file is part of the build system of a GAP kernel extension.
# Requires automake.
#
ACLOCAL_AMFLAGS = -I m4

BINARCHDIR = bin/$(GAPARCH)
GAPINSTALLLIB = $(abs_top_srcdir)/$(BINARCHDIR)/semigroups.so

lib_LTLIBRARIES = semigroups.la

semigroups_la_SOURCES =  src/pkg.cc src/converter.cc src/bipart.cc 
semigroups_la_SOURCES += src/uf.cc src/fropin.cc src/congpairs.cc
semigroups_la_SOURCES += src/semigrp.cc
semigroups_la_SOURCES += src/libsemigroups/src/blocks.cc
semigroups_la_SOURCES += src/libsemigroups/src/cong.cc
semigroups_la_SOURCES += src/libsemigroups/src/elements.cc
semigroups_la_SOURCES += src/libsemigroups/src/rws.cc
semigroups_la_SOURCES += src/libsemigroups/src/rwse.cc
semigroups_la_SOURCES += src/libsemigroups/src/semigroups.cc
semigroups_la_SOURCES += src/libsemigroups/src/uf.cc

semigroups_la_CXXFLAGS = -std=c++11 -O3 -g 

semigroups_la_CPPFLAGS = $(GAP_CPPFLAGS) -DCONFIG_H
# Note that the latter is only for GAP 4.4.12
semigroups_la_LDFLAGS = -module -avoid-version

if SYS_IS_CYGWIN
semigroups_la_LDFLAGS += -no-undefined -version-info 0:0:0 -Wl,$(GAPROOT)/bin/$(GAPARCH)/gap.dll
endif
all-local: semigroups.la
	$(mkdir_p) $(top_srcdir)/$(BINARCHDIR)
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
