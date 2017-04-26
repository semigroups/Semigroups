# If a command fails, exit this script with an error code
set -e

# Display version of compiler
$CXX --version

cd ..

# Assume: linting and running GAP are mutually-exclusive test suites

if [ ! -z "$LINT" ]; then

  sudo pip install cpplint
  git clone -b master --depth=1 https://github.com/james-d-mitchell/gaplint gaplint
  mkdir gap gap/.git gap/pkg
  mv Semigroups gap/pkg/semigroups

elif [ ! -z "$GAP_BRANCH" ]; then

  echo -e "\nInstalling GAP..."
  git clone -b $GAP_BRANCH --depth=1 https://github.com/gap-system/gap.git gap
  cd gap
  ./autogen.sh
  ./configure --with-gmp=system $GAP_FLAGS
  make
  mkdir pkg
  cd pkg
  PKG_DIR=`pwd`
  mv ../../Semigroups semigroups

  echo -e "\nDownloading libsemigroups..."
  cd semigroups/src
  git clone https://github.com/james-d-mitchell/libsemigroups.git libsemigroups
  cd libsemigroups
  LIBSEMIGROUPS_VERS=`git tag | grep "v\d\+.\d\+.\d\+" | tail -n 1`
  echo "Checking out libsemigroups version $LIBSEMIGROUPS_VERS..."
  git checkout $LIBSEMIGROUPS_VERS
  cd ../..

  echo -e "\nCompiling the Semigroups package..."
  ./autogen.sh
  ./configure $PKG_FLAGS
  make

  echo -e "\nCloning the Digraphs package..."
  cd $PKG_DIR
  git clone https://github.com/gap-packages/Digraphs.git digraphs

  if [ ! -z "$COVERAGE" ]; then
    echo -e "\nDownloading the profiling package..."
    git clone https://github.com/gap-packages/profiling.git profiling
    cd profiling
    PROFILING_VERS=`git tag | grep "v\d\+.\d\+.\d\+" | tail -n 1`
    echo "Checking out profiling version $PROFILING_VERS..."
    git checkout $PROFILING_VERS
    ./autogen.sh
    ./configure $PKG_FLAGS
    make
    cd ..
  fi

  INSTALL_PKG () {
    echo -e "\nDownloading $1..."
    cd $PKG_DIR
    git clone https://github.com/gap-packages/$1.git $1
    cd semigroups
    VERSION=v`grep "\"$1\"" PackageInfo.g | awk -F'"' '{print $4}' | cut -c3-`
    cd ../$1
    echo "Checking out $1 $VERSION..."
    git checkout $VERSION
    if $2; then
      ./autogen.sh
      ./configure $PKG_FLAGS
      make
    fi
    cd ..
  }

  INSTALL_PKG "genss" false
  INSTALL_PKG "io"    true
  INSTALL_PKG "orb"   true

  echo -e "\nDownloading $GAPDOC..."
  cd $PKG_DIR
  curl -O https://www.gap-system.org/pub/gap/gap4/tar.gz/packages/$GAPDOC.tar.gz
  tar xzf $GAPDOC.tar.gz
  rm $GAPDOC.tar.gz
fi
