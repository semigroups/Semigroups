# If a command fails, exit this script with an error code
set -e

# Display version of compiler
$CXX --version

# Store this directory
SEMIDIR=$(pwd)

# Get libsemigroups if appropriate
if [ -d src ]
then
    cd src
    git clone -b $LIBSEMIGROUPS_BR --depth=1 https://github.com/james-d-mitchell/libsemigroups.git
    cd ..
fi
cd ..

# Download and compile GAP
git clone -b $GAP_BRANCH --depth=1 https://github.com/$GAP_FORK/gap.git
cd gap
./configure --with-gmp=system $GAP_FLAGS
make
mkdir pkg
cd ..

# Compile the Semigroups package
mv $SEMIDIR gap/pkg/semigroups
cd gap/pkg/semigroups
if [ -d src ]
then
    ./autogen.sh
    ./configure $PKG_FLAGS
    make
fi
cd ../..

# Get the packages
cd pkg
echo "Downloading $GAPDOC..."
curl -LO http://www.gap-system.org/pub/gap/gap4/tar.gz/packages/$GAPDOC.tar.gz
tar xzf $GAPDOC.tar.gz
rm $GAPDOC.tar.gz
echo "Downloading $IO..."
curl -LO http://www.gap-system.org/pub/gap/gap4/tar.gz/packages/$IO.tar.gz
tar xzf $IO.tar.gz
rm $IO.tar.gz
cd $IO
./configure $PKG_FLAGS
make
cd ..
echo "Downloading $ORB..."
curl -LO http://www.gap-system.org/pub/gap/gap4/tar.gz/packages/$ORB.tar.gz
tar xzf $ORB.tar.gz
rm $ORB.tar.gz
cd $ORB
./configure $PKG_FLAGS
make
cd ..
echo "Downloading $GENSS..."
curl -LO http://www.gap-system.org/pub/gap/gap4/tar.gz/packages/$GENSS.tar.gz
tar xzf $GENSS.tar.gz
rm $GENSS.tar.gz
git clone -b $DIGRAPHS_BR --depth=1 https://github.com/gap-packages/Digraphs.git digraphs
cd digraphs
./autogen.sh
./configure $PKG_FLAGS
make
cd ../../..

# Get gaplint
echo "Downloading gaplint..."
hg clone https://bitbucket.org/james-d-mitchell/gaplint
