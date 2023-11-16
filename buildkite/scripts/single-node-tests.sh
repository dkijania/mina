#!/bin/bash

set -eo pipefail

# Don't prompt for answers during apt-get install
export DEBIAN_FRONTEND=noninteractive

sudo apt-get update
sudo apt-get install -y git apt-transport-https ca-certificates tzdata curl python3 python3-pip wget

git config --global --add safe.directory /workdir

source buildkite/scripts/export-git-env-vars.sh

sudo echo "deb [trusted=yes] http://packages.o1test.net bullseye ${MINA_DEB_RELEASE}" | sudo tee /etc/apt/sources.list.d/mina.list
sudo apt-get update

echo "Installing mina test suite package: mina-test-suite=${MINA_DEB_VERSION}"
sudo apt-get install --allow-downgrades -y mina-test-suite=${MINA_DEB_VERSION} mina-berkeley=${MINA_DEB_VERSION}

mina-command-line-tests --mina-path mina-daemon