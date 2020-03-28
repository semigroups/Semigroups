# If a command fails, exit this script with an error code
set -e

mv ../Semigroups $HOME/semigroups

################################################################################
# Install software necessary for linting: cpplint and gaplint
################################################################################

if [ "$SUITE" == "lint" ]; then

  # Install cpplint and gaplint
  sudo pip install cpplint
  sudo pip install gaplint

  # Move Semigroups package into a GAP folder structure, so that cpplint is happy
  mkdir $HOME/gap $HOME/gap/.git $HOME/gap/pkg
  mv $HOME/semigroups $HOME/gap/pkg/semigroups

  exit 0
fi

################################################################################
# Install software necessary for tests and coverage: GAP and packages
################################################################################ 

################################################################################
# Install GAP
echo -e "\nInstalling GAP..."
if [ "$GAP" == "required" ]; then
  GAP=v`grep "GAPVERS" $HOME/semigroups/PackageInfo.g | awk -F'"' '{print $2}'`
fi
echo -e "\nInstalling GAP $GAP into $GAPROOT..."
git clone -b $GAP --depth=1 https://github.com/gap-system/gap.git $GAPROOT
cd $GAPROOT
./autogen.sh
./configure --with-gmp=system $GAP_FLAGS
make -j4
mkdir pkg

################################################################################
# Move Semigroups into its proper location
mv $HOME/semigroups $GAPROOT/pkg/semigroups

################################################################################
# Install libsemigroups
cd $GAPROOT/pkg/semigroups
if [ "$SUITE" != "external-libsemigroups" ]; then
  ./prerequisites.sh
else
  # Autoreconf requires that this directory exists even if we don't use the
  # included libsemigroups . . .
  mkdir libsemigroups 
fi

# Common curl settings
CURL="curl --connect-timeout 5 --max-time 10 --retry 5 --retry-delay 0 --retry-max-time 40 -L"

################################################################################
# Install digraphs, genss, io, orb, and profiling
PKGS=( "digraphs" "genss" "io" "orb" )
if [ "$SUITE"  == "coverage" ]; then
  PKGS+=( "profiling" )
fi
if [ "$PACKAGES" == "latest" ]; then 
  PKGS+=( "datastructures" )
fi

for PKG in "${PKGS[@]}"; do
  cd $GAPROOT/pkg

  # Get the relevant version number
  if [ "$PACKAGES" == "latest" ] || [ "$PKG" == "profiling" ]; then
    VERSION=`$CURL -s "https://github.com/gap-packages/$PKG/releases/latest" | grep \<title\>Release | awk -F' ' '{print $2}'`
  else
    VERSION=`grep "\"$PKG\"" $GAPROOT/pkg/semigroups/PackageInfo.g | awk -F'"' '{print $4}' | cut -c3-`
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

################################################################################
# Install required GAP packages
cd $GAPROOT/pkg
echo -e "\nGetting the required GAP packages (smallgrp, transgrp, primgrp)..."
$CURL -O "https://www.gap-system.org/pub/gap/gap4pkgs/packages-required-master.tar.gz"
tar xf packages-required-master.tar.gz
rm packages-required-master.tar.gz
