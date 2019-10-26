#!/usr/bin/env bash
set -e

if [ "$#" -lt 1 ]
then
  echo "Usage: ./build-testnet-develop-release.sh [debug|release] [default|no-rgs]"
  exit 1
fi

if [ -z "$JENKINS_HOME" ]; then
  git pull
fi

CONSENSUS_TYPE=""
if [ ! -z "$2" ]; then 
  CONSENSUS_TYPE="$2"
else
  CONSENSUS_TYPE="default"
fi

PATH="$PATH:/usr/local/bin" git lfs install
PATH="$PATH:/usr/local/bin" git lfs pull

VERSION=`git rev-parse --verify HEAD`

./scripts/build-all-docker.sh $VERSION $1 $CONSENSUS_TYPE

echo "Finished building and pushing develop release with tag $VERSION with consensus $CONSENSUS_TYPE"
