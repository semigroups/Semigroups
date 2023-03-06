#!/bin/bash

set -e

notice_it() {
    printf '\033[32m%s\033[0m\n' "$*"
}

error() {
    printf '\033[31m%s\033[0m\n' "$*"
    exit 1
}

SEMI_DIR=`dirname "$0"`
LIBS_DIR="$SEMI_DIR/libsemigroups"

# Get libsemigroups version from file
if [ -f "$SEMI_DIR/.LIBSEMIGROUPS_VERSION" ]; then
  VERS=`tr -d '\n' < $SEMI_DIR/.LIBSEMIGROUPS_VERSION`
else
  error "Error, cannot find $SEMI_DIR/.LIBSEMIGROUPS_VERSION"
fi

notice_it "libsemigroups v$VERS is required by this version of Semigroups"

if [ -f "config.log" ] ; then
  if grep -q "\.\/configure.*\-\-with\-external\-libsemigroups" config.log ; then
    notice_it "Configuration flag \"--with-external-libsemigroups\" found in config.log"
    notice_it "Not downloading libsemigroups, no further prerequisites required"
    exit 0
  fi
elif [[ ! -z "${DO_NOT_DOWNLOAD_LIBSEMIGROUPS}" ]]; then
  notice_it "Environment variable DO_NOT_DOWNLOAD_LIBSEMIGROUPS is defined"
  notice_it "Not downloading libsemigroups, no further prerequisites required"
  exit 0
fi

if [ -d "$LIBS_DIR" ] && [ "$(ls -A $LIBS_DIR)" ]; then
  notice_it "The libsemigroups directory exists and is non-empty"
  if [ -f "$LIBS_DIR/.VERSION" ]; then
    notice_it "The file libsemigroups/.VERSION is present"
    INSTALLED=`tr -d '\n' < "$LIBS_DIR/.VERSION"`
  elif [ -f "$LIBS_DIR/etc/version-number.sh" ]; then
    notice_it "Getting version number via etc/version-number.sh in libsemigroups"
    cd "$LIBS_DIR"
    INSTALLED=`etc/version-number.sh`
    cd "$SEMI_DIR"
  else
    error "Error, it is not possible to determine the libsemigroups version"
  fi
  notice_it "The installed version of libsemigroups is v$INSTALLED"
  LEAST=`echo -e "$VERS\n$INSTALLED" | sort -V | head -n1`
  if [[ "$VERS" != "$LEAST" ]]; then
    error "Error, the installed version of libsemigroups is too old"
  fi
  exit 0
fi

# Download libsemigroups
notice_it "Downloading libsemigroups v$VERS into $LIBS_DIR..."
curl -L -O "https://github.com/libsemigroups/libsemigroups/releases/download/v$VERS/libsemigroups-$VERS.tar.gz"
tar -xzf "libsemigroups-$VERS.tar.gz" && rm -f "libsemigroups-$VERS.tar.gz" && mv "libsemigroups-$VERS" "$LIBS_DIR"
