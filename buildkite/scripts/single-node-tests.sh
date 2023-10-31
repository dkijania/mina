#!/bin/bash

set -eo pipefail

# Don't prompt for answers during apt-get install
export DEBIAN_FRONTEND=noninteractive

sudo apt-get update
sudo apt-get install -y git apt-transport-https ca-certificates tzdata curl

TESTNET_NAME="berkeley"

git config --global --add safe.directory /workdir

source buildkite/scripts/export-git-env-vars.sh

#test executive dependency
echo "deb [trusted=yes] https://apt.releases.hashicorp.com $MINA_DEB_CODENAME main" | sudo tee /etc/apt/sources.list.d/hashicorp.list
sudo apt-get update
sudo apt-get install -y "terraform"

echo "Installing mina daemon package: mina-${TESTNET_NAME}=${MINA_DEB_VERSION}"
echo "deb [trusted=yes] http://packages.o1test.net $MINA_DEB_CODENAME $MINA_DEB_RELEASE" | sudo tee /etc/apt/sources.list.d/mina.list
sudo apt-get update
sudo apt-get install --allow-downgrades -y "mina-${TESTNET_NAME}=${MINA_DEB_VERSION}" "mina-test-executive=${MINA_DEB_VERSION}"

mkdir -p _build/default/src/app/cli/src
sudo cp /usr/local/bin/mina _build/default/src/app/cli/src/mina.exe

echo "--- Run tests"
./mina-command-line-tests.exe
