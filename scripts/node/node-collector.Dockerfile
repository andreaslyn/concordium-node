ARG universal_image_name
FROM $universal_image_name AS build

FROM ubuntu:20.04
ARG build_profile
# TODO only install actually used packages.
RUN apt-get update && \
    apt-get install -y ca-certificates libpq-dev && \
    rm -rf /var/lib/apt/lists/*
COPY --from=build "/target/${build_profile}/node-collector" /node-collector
COPY --from=build /target/start.sh /start.sh
ENTRYPOINT ["/start.sh"]
