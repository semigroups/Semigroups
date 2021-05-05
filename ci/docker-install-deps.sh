#!/usr/bin/env bash

# If a command fails, exit this script with an error code
set -e
set -o pipefail

# This script is intended to be run inside the docker container
# jamesdbmitchell/gap-docker:version-?.?.?

if [ "$SUITE" != "test" ] && [ "$SUITE" != "coverage" ]; then
  echo -e "\nError, unrecognised Travis suite: $SUITE"
  exit 1
fi

echo -e "\nInstalling dependencies . . . "
sudo apt-get --yes update
sudo apt-get --yes upgrade
sudo apt-get install curl libtool git --yes

echo -e "\nFixing permissions . . . "
sudo chown -R gap: $GAP_HOME/pkg/semigroups

################################################################################
# Install libsemigroups
cd $GAP_HOME/pkg/semigroups
if [ "$SUITE" != "external-libsemigroups" ]; then
  ./prerequisites.sh
else
  # Autoreconf requires that this directory exists even if we don't use the
  # included libsemigroups . . .
  mkdir libsemigroups 
fi

################################################################################
# Compile Semigroups package

echo -e "\nCompiling the Semigroups package..."
cd $GAP_HOME/pkg/semigroups
./autogen.sh
./configure $PKG_FLAGS $EXTRA_PKG_FLAGS --disable-hpcombi
make -j4

# Common curl settings
CURL="curl --connect-timeout 5 --max-time 10 --retry 5 --retry-delay 0 --retry-max-time 40 -L"

################################################################################
# Install digraphs, genss, io, orb, images, and profiling
PKGS=( "digraphs" "genss" "io" "orb" "images" "datastructures")
if [ "$SUITE"  == "coverage" ]; then
  PKGS+=( "profiling" )
fi
# We now need a newer GAPDoc than the one included in the Docker container for GAP 4.10.2
if [ "$GAP_VERSION" == "4.10.2" ]; then
  PKGS+=( "GAPDoc" )
fi

for PKG in "${PKGS[@]}"; do
  cd $GAP_HOME/pkg

  # Get the relevant version number
  if [ "$PACKAGES" == "latest" ] || [ "$PKG" == "profiling" ]; then
    VERSION=`$CURL -s "https://github.com/gap-packages/$PKG/releases/latest" | grep \<title\>Release | awk -F' ' '{print $2}'`
  else
    VERSION=`grep "\"$PKG\"" $GAP_HOME/pkg/semigroups/PackageInfo.g | awk -F'"' '{print $4}' | cut -c3-`
  fi

  if [ -z $VERSION ]; then
    echo -e "\nCould not determine the version number of the package $PKG!! Aborting..."
    exit 1
  fi

  # This can be removed when there is no GAPDoc special case for GAP 4.10.2
  if [ "$PKG" == "GAPDoc" ]; then
    URL="http://www.math.rwth-aachen.de/~Frank.Luebeck/GAPDoc/GAPDoc-$VERSION.tar.gz"
  else
    URL="https://github.com/gap-packages/$PKG/releases/download/v$VERSION/$PKG-$VERSION.tar.gz"
  fi

  echo -e "\nDownloading $PKG-$VERSION ($PACKAGES version), from URL:\n$URL"
  $CURL "$URL" -o $PKG-$VERSION.tar.gz
  tar xf $PKG-$VERSION.tar.gz && rm $PKG-$VERSION.tar.gz

  if [ -f $PKG-$VERSION/configure ]; then
    if [ "$PKG" == "orb" ] || [ "$PKG" == "datastructures" ]; then
      cd $PKG-$VERSION && ./configure && make # orb doesn't accept package flags
    else
      cd $PKG-$VERSION && ./configure $PKG_FLAGS && make
    fi
  fi
done
