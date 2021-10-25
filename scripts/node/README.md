# Dockerized components

The dockerfiles in this folder are used to build images for each of the components

- `node`
- `bootstrapper`
- `node-collector`
- `node-collector-backend`

All the components are compiled (in both release and debug) in a monolithic image from the dockerfile `universal.Dockerfile`.
The other dockerfiles just extract the individual binaries from this image, install dependencies, and declare exposed ports.

Except for `node-collector-backend`, the images are currently only used on devnet (which at the time of this writing is not in working state).

To run the node, the genesis file `genesis.dat` needs to be placed at the data path when the node starts.
The image built from `genesis.Dockerfile` contains just this file and a script for copying it into the correct path at load time.

## docker-compose

The following example shows a minimal setup of a node and an accompanying collector.

TODO NOT YET TESTED

```yaml
version: '3'
services:
  genesis:
    container_name: node-init-genesis
    image: ${GENESIS_IMAGE}
    entrypoint: cp /genesis.dat /data/genesis.dat
    networks:
    - none
    volumes:
    - data:/data
  node:
    container_name: node
    image: ${NODE_IMAGE}
    depends_on:
    - genesis
    networks:
    - concordium
    environment:
    - CONCORDIUM_NODE_BOOTSTRAP_FIRST_NODE=bootstrap.mainnet.concordium.software:8888
    - CONCORDIUM_NODE_DATA_DIR=/var/lib/concordium/data
    - CONCORDIUM_NODE_CONFIG_DIR=/var/lib/concordium/config
    - CONCORDIUM_NODE_PROMETHEUS_METRICS_SERVER=1
    - CONCORDIUM_NODE_RPC_SERVER_ADDR=0.0.0.0
    - EXTRA_ARGS=--no-dnssec
    ports:
    - "8888:8888"
    - "10000:10000"
    volumes:
    - data:/var/lib/concordium/data
    - config:/var/lib/concordium/config
  node-collector:
    container_name: node-collector
    image: ${NODE_COLLECTOR_IMAGE}
    depends_on:
    - node
    networks:
    - concordium
    environment:
    - CONCORDIUM_COLLECTOR_URL=http://dashboard.mainnet.concordium.software/nodes/post
    - CONCORDIUM_COLLECTOR_GRPC_HOST=http://node:10000
    - CONCORDIUM_COLLECTOR_NODE_NAME=${NODE_NAME}
volumes:
  data:
  config:
networks:
  concordium:
```

## Kubernetes

TODO...
