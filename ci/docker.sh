#!/usr/bin/env bash

# If a command fails, exit this script with an error code
set -e
set -o pipefail

# This file only exists because it seems not possible to run both the scripts
# below directly in launch-docker-container.sh

$GAP_HOME/pkg/semigroups/ci/install-in-docker-container.sh
$GAP_HOME/pkg/semigroups/ci/run-tests-in-docker-container.sh
