#!/usr/bin/env bash

# If a command fails, exit this script with an error code
set -e
set -o pipefail

$GAP_HOME/pkg/semigroups/ci/docker-install-deps.sh
$GAP_HOME/pkg/semigroups/ci/docker-test.sh
