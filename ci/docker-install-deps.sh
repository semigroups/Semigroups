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
sudo apt-get install libtool curl --yes

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
./configure $PKG_FLAGS $EXTRA_PKG_FLAGS
make -j4

# Common curl settings
CURL="curl --connect-timeout 5 --max-time 10 --retry 5 --retry-delay 0 --retry-max-time 40 -L"

################################################################################
# Install digraphs, genss, io, orb, images, and profiling
PKGS=( "digraphs" "genss" "io" "orb" "images" "datastructures")
if [ "$SUITE"  == "coverage" ]; then
  PKGS+=( "profiling" )
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

  URL="https://github.com/gap-packages/$PKG/releases/download/v$VERSION/$PKG-$VERSION.tar.gz"
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
