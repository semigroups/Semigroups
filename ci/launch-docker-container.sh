#!/bin/bash
set -e

if [ "$GAP_VERSION" == "master" ]; then
  CONTAINER_NAME="gapsystem/gap-docker-master"
else
  CONTAINER_NAME_BASE="jamesdbmitchell/gap-docker-minimal"
  if [ "$ABI" == "32" ]; then
    CONTAINER_NAME_BASE="$CONTAINER_NAME_BASE-32"
  fi
  CONTAINER_NAME="$CONTAINER_NAME_BASE:version-$GAP_VERSION"
fi

# Pull the docker container
docker pull $CONTAINER_NAME 

# Start the docker container detached
ID=$(docker run --rm -i -d -e PACKAGES -e ABI "$CONTAINER_NAME")

GAP_HOME=$(docker exec $ID bash -c 'echo "$GAP_HOME"')

# Copy the Semigroups directory into the container
docker cp . $ID:$GAP_HOME/pkg/semigroups

# Run ci/install-in-docker-container.sh + run-tests.sh in the running
# container
docker exec -i $ID "$GAP_HOME/pkg/semigroups/ci/docker.sh" ; exit

# Attach to the container
docker attach $ID
