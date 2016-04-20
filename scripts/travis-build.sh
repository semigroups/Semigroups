# If a command fails, exit this script with an error code
set -e

# Display version of compiler
$CXX --version

# Store this directory
SEMIDIR=$(pwd)

# Get semigroups++ if appropriate
if [ -d src ]
then
    cd src
    git clone -b 0.1 --depth=1 https://github.com/james-d-mitchell/semigroupsplusplus.git
    mv semigroupsplusplus semigroups++
    cd ..
fi
cd ..

# Download and compile GAP
git clone -b master --depth=1 https://github.com/gap-system/gap.git
cd gap
./configure --with-gmp=system $GAP_CONFIGURE_FLAGS
make

# Get the packages
mkdir pkg
cd pkg
curl -O http://www.gap-system.org/pub/gap/gap4/tar.gz/packages/GAPDoc-1.5.1.tar.gz
tar xzf GAPDoc-1.5.1.tar.gz
rm GAPDoc-1.5.1.tar.gz
curl -O http://www.gap-system.org/pub/gap/gap4/tar.gz/packages/io-4.4.5.tar.gz
tar xzf io-4.4.5.tar.gz
rm io-4.4.5.tar.gz
cd io-4.4.5
./configure $PKG_CONFIGURE_FLAGS
make
cd ..
curl -O http://www.gap-system.org/pub/gap/gap4/tar.gz/packages/orb-4.7.5.tar.gz
tar xzf orb-4.7.5.tar.gz
rm orb-4.7.5.tar.gz
cd orb-4.7.5
./configure $PKG_CONFIGURE_FLAGS
make
cd ..
curl -O http://www.gap-system.org/pub/gap/gap4/tar.gz/packages/genss-1.6.3.tar.gz
tar xzf genss-1.6.3.tar.gz
rm genss-1.6.3.tar.gz
curl -O http://www.gap-system.org/pub/gap/gap4/tar.gz/packages/grape4r7.tar.gz
tar xzf grape4r7.tar.gz
rm grape4r7.tar.gz
cd grape
./configure $PKG_CONFIGURE_FLAGS
make
cd ..
hg clone https://james-d-mitchell@bitbucket.org/james-d-mitchell/digraphs -r 0.5.1
cd digraphs
./autogen.sh
./configure $PKG_CONFIGURE_FLAGS
make
cd ../../..
mv $SEMIDIR gap/pkg/semigroups
cd gap/pkg/semigroups
if [ -d src ]
then
    ./autogen.sh
    ./configure $PKG_CONFIGURE_FLAGS
    make
fi
cd ../..
