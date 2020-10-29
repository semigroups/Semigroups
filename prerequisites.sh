set -e

SEMI_DIR=`dirname "$0"`
LIBS_DIR="$SEMI_DIR/libsemigroups"
LIBSEMIGROUPS_BRANCH=fix-32-bit
LIBSEMIGROUPS_REPO=james-d-mitchell

# Get libsemigroups version from file
if [ -f $SEMI_DIR/.LIBSEMIGROUPS_VERSION ]; then
  VERS=`tr -d '\n' < $SEMI_DIR/.LIBSEMIGROUPS_VERSION`
else
  echo "Error, cannot find $SEMI_DIR/.LIBSEMIGROUPS_VERSION"
  exit 1
fi

echo "libsemigroups v$VERS is required by this version of Semigroups"

if [ -d $LIBS_DIR ] && [ "$(ls -A $LIBS_DIR)" ]; then
  echo "the $LIBS_DIR directory exists and is non-empty"
  if ! [ -f $LIBS_DIR/.VERSION ]; then
    echo "Error, it is not possible to determine the libsemigroups version"
    exit 2
  fi
  INSTALLED=`tr -d '\n' < $LIBS_DIR/.VERSION`
  echo "The installed version of libsemigroups is v$INSTALLED"
  LEAST=`echo -e "$VERS\n$INSTALLED" | sort -V | head -n1`
  if [[ "$VERS" != "$LEAST" ]]; then
    echo "Error, the installed version of libsemigroups is too old"
    exit 3
  fi
  exit 0
fi

if [ ! -z "$LIBSEMIGROUPS_REPO" ]; then
  echo "Downloading libsemigroups from $LIBSEMIGROUPS_REPO/$LIBSEMIGROUPS_BRANCH to $LIBS_DIR..."
  git clone -b $LIBSEMIGROUPS_BRANCH --depth=1 https://github.com/$LIBSEMIGROUPS_REPO/libsemigroups.git $LIBS_DIR
  cd $LIBS_DIR && ./autogen.sh && cd ..
else
  echo "Downloading libsemigroups v$VERS into $LIBS_DIR..."
  curl -L -O https://github.com/libsemigroups/libsemigroups/releases/download/v$VERS/libsemigroups-$VERS.tar.gz
  tar -xzf libsemigroups-$VERS.tar.gz && rm -f libsemigroups-$VERS.tar.gz && mv libsemigroups-$VERS $LIBS_DIR 
fi
