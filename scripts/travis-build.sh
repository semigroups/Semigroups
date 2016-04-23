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
    git clone -b $SEMIGROUPSPLUSPLUS_BR --depth=1 https://github.com/james-d-mitchell/semigroupsplusplus.git
    mv semigroupsplusplus semigroups++
    cd ..
fi
cd ..

# Download and compile GAP
git clone -b $GAP_BRANCH --depth=1 https://github.com/gap-system/gap.git
cd gap
./configure --with-gmp=system $GAP_FLAGS
make

# Get the packages
mkdir pkg
cd pkg
curl -O http://www.gap-system.org/pub/gap/gap4/tar.gz/packages/$GAPDOC.tar.gz
tar xzf $GAPDOC.tar.gz
rm $GAPDOC.tar.gz
curl -O http://www.gap-system.org/pub/gap/gap4/tar.gz/packages/$IO.tar.gz
tar xzf $IO.tar.gz
rm $IO.tar.gz
cd $IO
./configure $PKG_FLAGS
make
cd ..
curl -O http://www.gap-system.org/pub/gap/gap4/tar.gz/packages/$ORB.tar.gz
tar xzf $ORB.tar.gz
rm $ORB.tar.gz
cd $ORB
./configure $PKG_FLAGS
make
cd ..
curl -O http://www.gap-system.org/pub/gap/gap4/tar.gz/packages/$GENSS.tar.gz
tar xzf $GENSS.tar.gz
rm $GENSS.tar.gz
curl -O http://www.gap-system.org/pub/gap/gap4/tar.gz/packages/$GRAPE.tar.gz
tar xzf $GRAPE.tar.gz
rm $GRAPE.tar.gz
cd grape
./configure $PKG_FLAGS
make
cd ..
hg clone https://james-d-mitchell@bitbucket.org/james-d-mitchell/digraphs -r $DIGRAPHS_BR
cd digraphs
./autogen.sh
./configure $PKG_FLAGS
make
cd ../../..
mv $SEMIDIR gap/pkg/semigroups
cd gap/pkg/semigroups
if [ -d src ]
then
    ./autogen.sh
    ./configure $PKG_FLAGS
    make
fi
cd ../..
