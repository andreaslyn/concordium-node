#!/usr/bin/env bash

set -e

docker build -f scripts/base.Dockerfile -t 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/base:0.5 .

docker push 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/base:0.5

echo "DONE BUILDING base!"
