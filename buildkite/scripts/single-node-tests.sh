#!/bin/bash

set -eo pipefail

if [[ $# -ne 1 ]]; then
    echo "Usage: $0  <path-to-source-tests>"
    exit 1
fi

path=$1

# Don't prompt for answers during apt-get install
export DEBIAN_FRONTEND=noninteractive

sudo apt-get update
sudo apt-get install -y git apt-transport-https ca-certificates tzdata curl dune=2.7

case "$BUILDKITE_PULL_REQUEST_BASE_BRANCH" in
  rampup|berkeley|release/2.0.0|develop)
    TESTNET_NAME="berkeley"
  ;;
  *)
    TESTNET_NAME="mainnet"
esac

git config --global --add safe.directory /workdir

source buildkite/scripts/export-git-env-vars.sh

echo "Installing mina daemon package: mina-${TESTNET_NAME}=${MINA_DEB_VERSION}"
echo "deb [trusted=yes] http://packages.o1test.net $MINA_DEB_CODENAME $MINA_DEB_RELEASE" | sudo tee /etc/apt/sources.list.d/mina.list
sudo apt-get update
sudo apt-get install --allow-downgrades -y "mina-${TESTNET_NAME}=${MINA_DEB_VERSION}"

mkdir -p _build/default/src/app/cli/src
sudo cp /usr/local/bin/mina _build/default/src/app/cli/src/mina.exe

echo "--- Run tests"
dune exec "${path}" -- -v 
