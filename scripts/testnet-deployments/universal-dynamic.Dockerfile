ARG base_image_tag

# Build binaries.
FROM concordium/base:${base_image_tag} as build
COPY . /build
WORKDIR /build

# Compile consensus.
RUN stack build --stack-yaml concordium-consensus/stack.yaml

# Build...
# Note that feature 'profiling' implies 'static', so that should not be added.
ARG features='instrumentation,collector'
RUN cargo build --manifest-path concordium-node/Cargo.toml --features="${features}"
RUN cargo build --manifest-path concordium-node/Cargo.toml --release --features="${features}"

# Copy artifacts to '/out'.
# Enable brace expansion in the following 'RUN' directive.
SHELL ["/bin/bash", "-c"]
RUN for profile in debug release; do \
        mkdir -p "/out/${profile}" && \
        cp \
            "/build/concordium-node/target/${profile}/"{concordium-node,p2p_bootstrapper-cli,node-collector,node-collector-backend} \
            "/out/${profile}/"; \
    done && \
    cp /build/scripts/start.sh /out/start.sh && \
    mkdir -p /out/libs && \
    cp /build/concordium-base/rust-src/target/release/*.so /out/libs && \
    cp /build/concordium-consensus/.stack-work/install/x86_64-linux/*/*/lib/x86_64-linux-ghc-*/libHS*.so /out/libs && \
    cp /build/concordium-consensus/smart-contracts/lib/*.so /out/libs && \
    cp /root/.stack/snapshots/x86_64-linux/*/*/lib/x86_64-linux-ghc-*/libHS*.so /out/libs && \
    cp /opt/ghc/*/lib/*/*/libHS*.so /out/libs
