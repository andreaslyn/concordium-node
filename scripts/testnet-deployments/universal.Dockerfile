ARG base_image_tag
ARG static_libraries_image_tag

# Build static consensus libraries.
FROM concordium/static-libraries:${static_libraries_image_tag} as static-builder
COPY . /build
ARG ghc_version
RUN GHC_VERSION="${ghc_version}" \
        /build/scripts/build-static-libraries.sh

# Build binaries.
FROM concordium/base:${base_image_tag} as build
COPY . /build
# Build in both release and debug mode.
ARG consensus_profiling=false
ENV CONSENSUS_PROFILING="${consensus_profiling}"
RUN /build/scripts/build-binaries.sh "instrumentation,collector" "release" && \
    mkdir -p /out/release && \
    cp /build/concordium-node/target/release/concordium-node \
       /build/concordium-node/target/release/p2p_bootstrapper-cli \
       /build/concordium-node/target/release/node-collector \
       /build/concordium-node/target/release/node-collector-backend \
       /out/release/ && \
    /build/scripts/build-binaries.sh "instrumentation,collector" && \
    mkdir -p /out/debug && \
    cp /build/concordium-node/target/debug/concordium-node \
       /build/concordium-node/target/debug/p2p_bootstrapper-cli \
       /build/concordium-node/target/debug/node-collector \
       /build/concordium-node/target/debug/node-collector-backend \
       /out/debug/ && \
    cp /build/scripts/start.sh /out/start.sh

FROM ubuntu:20.04
COPY --from=build /out /out
