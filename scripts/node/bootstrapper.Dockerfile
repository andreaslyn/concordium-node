ARG universal_image_name
FROM $universal_image_name AS build

FROM ubuntu:20.04
ARG build_profile
EXPOSE 8888
# TODO only install actually used packages.
RUN apt-get update && \
    apt-get install -y libpq-dev && \
    rm -rf /var/lib/apt/lists/*
COPY --from=build "/out/${build_profile}/p2p_bootstrapper-cli" /p2p_bootstrapper-cli
COPY --from=build /out/start.sh /start.sh
ENTRYPOINT ["/start.sh"]
