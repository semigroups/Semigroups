#!/bin/sh -ex
#
# Semigroups package for GAP
#
# This file is part of the build system of a GAP kernel extension.
# Requires GNU autoconf, GNU automake and GNU libtool.
#
autoreconf -vif `dirname "$0"`

# autoconf 2.69 has a bug where autoreconf does not copy config.guess
# and config.sub even though configure.ac needs them, unless automake is
# being used. We work around this by forcing a call to automake if one
# or both of config.sub and config.guess are missing.
if ! test -x config.guess -a -x config.sub ; then
  automake -acf 2> /dev/null || :
fi

# There is a second bug in autoconf 2.69 where the generated configure
# script complains about install-sh not being there (even though it does
# not actually need it). As a workaround, we just provide an empty file
# instead. Since newer autoconf versions such as 2.71 are not affected
# by the bug, we add a test to limit when this workaround is applied
if fgrep -q ac_aux_dir/install-sh configure ; then
  touch install-sh
fi
