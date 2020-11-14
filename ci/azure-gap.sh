#!/bin/bash
set -e

export CONTAINER_NAME="jamesdbmitchell/gap-docker-minimal:version-$GAP_VERSION"

# Pull the docker container
docker pull $CONTAINER_NAME 

# Start the docker container detached
ID=$(docker run --rm -i -d -e SUITE -e PACKAGES $CONTAINER_NAME)

GAP_HOME=$(docker exec $ID bash -c 'echo "$GAP_HOME"')

# Copy the digraphs directory into the container
docker cp . $ID:$GAP_HOME/pkg/semigroups

# Run the ci/docker-test.sh script in the running container
docker exec -i $ID "$GAP_HOME/pkg/semigroups/ci/docker-test.sh" ; exit

# Attach to the container
docker attach $ID
