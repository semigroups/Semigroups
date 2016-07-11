# If a command fails, exit this script with an error code
set -e

# Display version of compiler
$CXX --version

# Store this directory
SEMIDIR=$(pwd)

# Get semigroups++ if appropriate
echo -en 'travis_fold:start:GetSemigroupsPP\r'
if [ -d src ]
then
    cd src
    git clone -b $SEMIGROUPSPLUSPLUS_BR --depth=1 https://github.com/james-d-mitchell/semigroupsplusplus.git
    mv semigroupsplusplus semigroups++
    cd ..
fi
cd ..
echo -en 'travis_fold:end:GetSemigroupsPP\r'

# Download and compile GAP
echo -en 'travis_fold:start:InstallGAP\r'
git clone -b $GAP_BRANCH --depth=1 https://github.com/$GAP_FORK/gap.git
cd gap
./configure --with-gmp=system $GAP_FLAGS
make
mkdir pkg
cd ..
echo -en 'travis_fold:end:InstallGAP\r'

# Compile the Semigroups package
echo -en 'travis_fold:start:BuildSemigroups\r'
mv $SEMIDIR gap/pkg/semigroups
cd gap/pkg/semigroups
if [ -d src ]
then
    ./autogen.sh
    ./configure $PKG_FLAGS
    make
fi
cd ../..
echo -en 'travis_fold:end:BuildSemigroups\r'

# Get the packages
echo -en 'travis_fold:start:InstallPackages\r'
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
git clone -b $DIGRAPHS_BR --depth=1 https://github.com/gap-packages/Digraphs.git digraphs
cd digraphs
./autogen.sh
./configure $PKG_FLAGS
make
cd ../../..
echo -en 'travis_fold:end:InstallPackages\r'
