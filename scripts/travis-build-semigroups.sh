# If a command fails, exit this script with an error code
set -e

################################################################################
# Compile Semigroups package
if [ "$SUITE" != "lint" ]; then
  echo -e "\nCompiling the Semigroups package..."
  cd $GAPROOT/pkg/semigroups
  ./autogen.sh
  ./configure $PKG_FLAGS
  make
fi
