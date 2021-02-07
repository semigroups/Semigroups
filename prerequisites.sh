#!/bin/bash

set -e

SEMI_DIR=`dirname "$0"`
LIBS_DIR="$SEMI_DIR/libsemigroups"

# Get libsemigroups version from file
if [ -f "$SEMI_DIR/.LIBSEMIGROUPS_VERSION" ]; then
  VERS=`tr -d '\n' < $SEMI_DIR/.LIBSEMIGROUPS_VERSION`
else
  echo "Error, cannot find $SEMI_DIR/.LIBSEMIGROUPS_VERSION"
  exit 1
fi

echo "libsemigroups v$VERS is required by this version of Semigroups"

if [ -d "$LIBS_DIR" ] && [ "$(ls -A $LIBS_DIR)" ]; then
  echo "the libsemigroups directory exists and is non-empty"
  if [ -f "$LIBS_DIR/.VERSION" ]; then
    echo "The file libsemigroups/.VERSION is present"
    INSTALLED=`tr -d '\n' < "$LIBS_DIR/.VERSION"`
  elif [ -f "$LIBS_DIR/etc/version-number.sh" ]; then
    echo "The file libsemigroups/.VERSION is not present; recreating it..."
    cd "$LIBS_DIR"
    INSTALLED=`etc/version-number.sh`
    echo "$INSTALLED" > .VERSION
    cd "$SEMI_DIR"
  else
    echo "Error, it is not possible to determine the libsemigroups version"
    exit 2
  fi
  echo "The installed version of libsemigroups is v$INSTALLED"
  LEAST=`echo -e "$VERS\n$INSTALLED" | sort -V | head -n1`
  if [[ "$VERS" != "$LEAST" ]]; then
    echo "Error, the installed version of libsemigroups is too old"
    exit 3
  fi
  exit 0
fi

# Download libsemigroups
echo  "Downloading libsemigroups v$VERS into $LIBS_DIR..."
curl -L -O "https://github.com/libsemigroups/libsemigroups/releases/download/v$VERS/libsemigroups-$VERS.tar.gz"
tar -xzf "libsemigroups-$VERS.tar.gz" && rm -f "libsemigroups-$VERS.tar.gz" && mv "libsemigroups-$VERS" "$LIBS_DIR"
