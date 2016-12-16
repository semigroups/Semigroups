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

semigroups_la_SOURCES =  src/gap.cc src/converter.cc src/bipart.cc 
semigroups_la_SOURCES += src/ufdata.cc src/fropin.cc src/congpairs.cc
semigroups_la_SOURCES += src/semifp.cc src/semigrp.cc
semigroups_la_SOURCES += src/semigroupsplusplus/elements.cc
semigroups_la_SOURCES += src/semigroupsplusplus/blocks.cc
semigroups_la_SOURCES += src/semigroupsplusplus/semigroups.cc 
semigroups_la_SOURCES += src/semigroupsplusplus/cong.cc
semigroups_la_CXXFLAGS = -std=c++11 -O3 -g -DNDEBUG

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
	@echo "SUCCESS!"

distclean-local:
	rm -rf bin/*

doc: doc/manual.six

doc/manual.six: doc/*.xml PackageInfo.g
	($(GAPROOT)/bin/gap.sh -A makedoc.g)

debug: CXXFLAGS = -g3 -O0 -std=c++11 -Wall -Wextra -Wno-unused-parameter -Wtype-limits -Wformat-security -Wpointer-arith -Wno-div-by-zero -Wreturn-type -Woverloaded-shift-op-parentheses -Wstring-conversion -Wswitch -Wsizeof-array-argument -Wloop-analysis -Wunused-label -ferror-limit=100 -fno-omit-frame-pointer -fcolor-diagnostics -fno-omit-frame-pointer -Wno-c++11-extensions -Wtautological-constant-out-of-range-compare -UNDEBUG -DDEBUG
debug: clean all

.PHONY: debug
