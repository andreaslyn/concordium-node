# Dockerized components

The dockerfiles in this folder are used to build images for each of the components

- `node`
- `bootstrapper`
- `node-collector`
- `node-collector-backend`

All the components are compiled (in both release and debug) in a monolithic image from the dockerfile `universal.Dockerfile`.
The other dockerfiles just extract the individual binaries from this image.
A start script is also added, system dependencies are installed, and relevant ports exposed.

The start script basically just translates env vars to CLI args and invokes the intended binary.
It is quite unpolished and will be improved when time permits.

Except the `node-collector-backend`, the images are currently only used on devnet (which at the time of this writing is not in working state).

To run the node, the genesis file `genesis.dat` needs to be placed at the data path when the node starts.
The image built from `genesis.Dockerfile` contains just this file and a script for copying it into the correct path at load time.

## docker-compose

The following example shows a minimal setup of a node and an accompanying collector.

TODO NOT YET TESTED

```yaml
version: '3'
services:
  genesis:
    image: 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/genesis:<tag>
    container_name: node-init-genesis
    environment:
    - DATA_DIR=/data
    networks:
    - none
    volumes:
    - data:/data
  node:
    image: 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/node:<tag>
    container_name: node
    depends_on:
    - genesis
    networks:
    - concordium
    environment:
    - MODE=basic
    - BOOTSTRAP_FIRST_NODE=bootstrap.mainnet.concordium.software:8888
    - DATA_DIR=/var/lib/concordium/data
    - CONFIG_DIR=/var/lib/concordium/config
    - PROMETHEUS_METRICS_SERVER=1
    - RPC_SERVER_ADDR=0.0.0.0
    - EXTRA_ARGS=--no-dnssec
    ports:
    - 8888:8888
    - 10000:10000
    volumes:
    - data:/var/lib/concordium/data
    - config:/var/lib/concordium/config
    entrypoint:
    - /start.sh
  node-collector:
    image: 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/node-collector:<tag>
    container_name: node-collector
    depends_on:
    - node
    networks:
    - concordium
    environment:
    - MODE=collector
    - COLLECTOR_URL=http://dashboard.mainnet.concordium.software/nodes/post
    - COLLECTOR_GRPC_HOST=http://node:10000
    - COLLECTOR_NODE_NAME=${NODE_NAME}
    entrypoint:
    - /start.sh
volumes:
  data:
  config:
networks:
  concordium:
```

## Kubernetes

TODO...
