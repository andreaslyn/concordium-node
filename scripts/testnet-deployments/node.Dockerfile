# syntax=docker/dockerfile:experimental

ARG universal_image_name

FROM $universal_image_name AS build
FROM ubuntu:20.04

ARG build_type

EXPOSE 8888  # listen port
EXPOSE 9090  # prometheus
EXPOSE 10000 # GRPC

# TODO only install actually used packages.
RUN apt-get update && \
    apt-get install -y unbound ca-certificates libpq-dev && \
    rm -rf /var/lib/apt/lists/*

COPY --from=build /out/$build_type/concordium-node /concordium-node
COPY --from=build /out/start.sh /start.sh

ENTRYPOINT ["/start.sh"]
