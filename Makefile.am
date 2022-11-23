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
  BUILT_SOURCES = bin/include/libsemigroups/libsemigroups.hpp
endif

AM_CPPFLAGS = @LIBSEMIGROUPS_CFLAGS@

BINARCHDIR = bin/$(GAPARCH)
GAPINSTALLLIB = $(abs_top_srcdir)/$(BINARCHDIR)/semigroups.so

lib_LTLIBRARIES = semigroups.la

semigroups_la_SOURCES =  src/bipart.cpp
semigroups_la_SOURCES += src/cong.cpp
semigroups_la_SOURCES += src/froidure-pin-base.cpp
semigroups_la_SOURCES += src/froidure-pin-bipart.cpp
semigroups_la_SOURCES += src/froidure-pin-bmat.cpp
semigroups_la_SOURCES += src/froidure-pin-fallback.cpp
semigroups_la_SOURCES += src/froidure-pin-matrix.cpp
semigroups_la_SOURCES += src/froidure-pin-max-plus-mat.cpp
semigroups_la_SOURCES += src/froidure-pin-min-plus-mat.cpp
semigroups_la_SOURCES += src/froidure-pin-pbr.cpp
semigroups_la_SOURCES += src/froidure-pin-pperm.cpp
semigroups_la_SOURCES += src/froidure-pin-transf.cpp
semigroups_la_SOURCES += src/pkg.cpp
semigroups_la_SOURCES += src/to_gap.cpp
semigroups_la_SOURCES += gapbind14/src/gapbind14.cpp

semigroups_la_CXXFLAGS = $(GAP_CFLAGS)   @LIBSEMIGROUPS_CFLAGS@ -std=gnu++14 -O3
semigroups_la_CPPFLAGS = $(GAP_CPPFLAGS) @LIBSEMIGROUPS_CFLAGS@
semigroups_la_CPPFLAGS += -I$(top_srcdir)/gapbind14/include

if HPCOMBI_CONSTEXPR_FUN_ARGS
semigroups_la_CPPFLAGS += -DHPCOMBI_CONSTEXPR_FUN_ARGS
endif

if LIBSEMIGROUPS_HPCOMBI_ENABLED
semigroups_la_CXXFLAGS += $(HPCOMBI_CXXFLAGS)
endif

semigroups_la_CFLAGS = $(GAP_CFLAGS)

if WITH_INCLUDED_LIBSEMIGROUPS
# FIXME(later) all the include paths should point into bin/include/ and not to
# the sources, or otherwise we should stop make installing into bin
if LIBSEMIGROUPS_HPCOMBI_ENABLED
semigroups_la_CPPFLAGS += -I$(top_srcdir)/libsemigroups/extern/HPCombi/include
semigroups_la_CPPFLAGS += -I$(top_srcdir)/libsemigroups/extern/HPCombi/include/fallback
endif
semigroups_la_CPPFLAGS += -I$(top_srcdir)/libsemigroups/extern/fmt-8.0.1/include
semigroups_la_CPPFLAGS += -I$(top_srcdir)/libsemigroups/include
endif
semigroups_la_CPPFLAGS += -DFMT_HEADER_ONLY

if KERNEL_DEBUG
semigroups_la_CPPFLAGS += -DDEBUG
else
semigroups_la_CPPFLAGS += -DNDEBUG
endif

semigroups_la_LDFLAGS = $(GAP_LDFLAGS) -module -avoid-version

semigroups_la_LIBADD = @LIBSEMIGROUPS_LIBS@

if SYS_IS_CYGWIN
semigroups_la_LDFLAGS += -no-undefined -version-info 0:0:0 -Wl,$(GAPROOT)/bin/$(GAPARCH)/gap.dll
endif

# the following is only run if BUILT_SOURCES is wound up
bin/include/libsemigroups/libsemigroups.hpp:
	$(MAKE) -C libsemigroups install

all-local: semigroups.la
	$(mkdir_p) $(top_srcdir)/$(BINARCHDIR)
	rm -f $(GAPINSTALLLIB)  # workaround for macOS code signing
if SYS_IS_CYGWIN
	cp .libs/semigroups.dll $(GAPINSTALLLIB)
if WITH_INCLUDED_LIBSEMIGROUPS
# Cygwin will only look in this directory for dlls
	cp libsemigroups/.libs/cygsemigroups-*.dll $(GAPROOT)/.libs
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

lint:
	etc/gaplint.sh
	etc/cpplint.sh

format:
	clang-format -i src/*.hpp src/*.cpp

superclean: distclean
	git clean -xdf --exclude *.swp --exclude *.swo
.PHONY: superclean
