#
# This Makefile serves two purposes:
#
# 1) If the user types "make" without having run "configure", we suggest
#    running configure.
#
# 2) Our build system is written for GNU make, and makes liberal use of its
#    features. We therefore put it into "GNUmakefile", which is picked up by
#    GNU make, but ignored by other make versions, such as BSD make. Thus, if
#    the user has BSD make, it will run this Makefile instead -- and we inform
#    them that they need to use GNU make to compile Semigroups.
#
# To learn more about the GAP build system, see README.buildsys.md
#
.DEFAULT:
	@if test -f GNUmakefile ; then \
	    printf "Please use GNU make to build Semigroups (try 'gmake' or 'gnumake')\n" ; \
	  else \
		printf "You need to run "; \
		if ! test -f configure ; then \
			printf "./autogen.sh then "; \
		fi; \
		printf "./configure before make (please refer to README.md or Chapter 2 of the Semigroups package documentation for details)\n" ; \
	  fi
	@exit 1
all: .DEFAULT
