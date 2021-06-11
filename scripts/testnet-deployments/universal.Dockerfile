ARG base_image_tag
ARG static_libraries_image_tag

# Build static consensus libraries.
FROM concordium/static-libraries:${static_libraries_image_tag} as static-builder
COPY . /build
ARG ghc_version
RUN GHC_VERSION="${ghc_version}" \
      /build/scripts/static-libraries/build-static-libraries.sh

# Build binaries.
FROM concordium/base:${base_image_tag} as build
COPY . /build
WORKDIR /build

# Copy static libraries that were built by the static-builder into the correct place
# (/build/concordium-node/deps/static-libs/linux).
ARG ghc_version
COPY --from=static-builder "/build/static-consensus-${ghc_version}.tar.gz" /tmp/static-consensus.tar.gz
RUN tar -C /tmp -xf /tmp/static-consensus.tar.gz && \
    mkdir -p /build/concordium-node/deps/static-libs && \
    mv /tmp/target /build/concordium-node/deps/static-libs/linux && \
    rm /tmp/static-consensus.tar.gz

# Build in both release and debug mode.
ARG consensus_profiling=false
ENV CONSENSUS_PROFILING="${consensus_profiling}"
RUN /build/scripts/build-binaries.sh "instrumentation,collector" "release" && \
    mkdir -p /out/release && \
    cp /build/concordium-node/target/release/{concordium-node,p2p_bootstrapper-cli,node-collector,node-collector-backend} /out/release/ && \
    /build/scripts/build-binaries.sh "instrumentation,collector" && \
    mkdir -p /out/debug && \
    cp /build/concordium-node/target/debug/{concordium-node,p2p_bootstrapper-cli,node-collector,node-collector-backend} /out/debug/ && \
    cp /build/scripts/start.sh /out/start.sh

FROM ubuntu:20.04
COPY --from=build /out /out
