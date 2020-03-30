# syntax=docker/dockerfile:experimental
FROM 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/base:0.11 as build
ARG consensus_type
ENV CONSENSUS_TYPE=$consensus_type
ARG consensus_profiling=false
ENV CONSENSUS_PROFILING=$consensus_profiling
COPY . /build-project
WORKDIR /build-project
COPY ./scripts/init.build.env.sh ./init.build.env.sh
COPY ./scripts/start.sh ./start.sh
COPY ./genesis-data ./genesis-data
COPY ./scripts/build-binaries.sh ./build-binaries.sh
ENV LD_LIBRARY_PATH=/usr/local/lib
RUN --mount=type=ssh ./init.build.env.sh
# Build P2P client
RUN --mount=type=ssh ./build-binaries.sh "collector,staging_net" release && \
    strip /build-project/target/release/p2p_client-cli && \
    strip /build-project/target/release/node-collector && \
    cp /build-project/target/release/p2p_client-cli /build-project/ && \
    cp /build-project/target/release/node-collector /build-project/ && \
    cd /build-project/genesis-data && \
    tar -xf 20-bakers.tar.gz && \
    cd genesis_data && \
    sha256sum genesis.dat && \
    cp genesis.dat /build-project/
# P2P client is now built
FROM 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/base:0.11 as haskell-build
COPY ./CONSENSUS_VERSION /CONSENSUS_VERSION
# Build middleware and concordium-client
RUN --mount=type=ssh pacman -Syy --noconfirm openssh && \
    mkdir -p -m 0600 ~/.ssh && ssh-keyscan gitlab.com >> ~/.ssh/known_hosts && \
    git clone git@gitlab.com:Concordium/consensus/simple-client.git && \
    cd simple-client && \
    git checkout 9cb83d4213c5dd16ba415d0b4d9be29d87695b34 && \
    git submodule update --init --recursive && \
    mkdir -p ~/.stack/global-project/ && \
    echo -e "packages: []\nresolver: $(cat stack.yaml | grep ^resolver: | awk '{ print $NF }')" > ~/.stack/global-project/stack.yaml && \
    curl -sSL https://get.haskellstack.org/ | sh && \
    curl -s "https://s3-eu-west-1.amazonaws.com/static-libraries.concordium.com/static-consensus-binaries-$(cat /CONSENSUS_VERSION).tar.gz" -O && \
    tar -xf static-consensus-binaries-$(cat /CONSENSUS_VERSION).tar.gz && \
    mv binaries /genesis-binaries && \
    ./build-deps.sh && \
    for f in $(find . -type f -name package.yaml); do sed -i -e 's/[\s]*ld-options://g' -e 's/[\s]*- -static//g' $f; done && \
    ./stack build --flag "simple-client:middleware" && \
    mkdir -p /libs && \
    cp extra-libs/* /libs/ && \
    cp .stack-work/dist/*/*/build/middleware/middleware /middleware && \
    cp .stack-work/dist/*/*/build/concordium-client/concordium-client /concordium-client-bin && \
    strip /middleware && \
    strip /concordium-client-bin && \
    cp scripts/testnet/config-add-account.sh /config-add-account.sh
# Middleware and concordium-client is now built

# Build oak compiler
#FROM 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/base-haskell:0.10 as oak-build
#WORKDIR /
#RUN mkdir -p -m 0600 ~/.ssh && ssh-keyscan gitlab.com >> ~/.ssh/known_hosts
#RUN --mount=type=ssh git clone git@gitlab.com:Concordium/oak/oak-compiler.git
#WORKDIR /oak-compiler
#RUN git checkout 7daba809397756b91f171568c46c26e9503e3956
#RUN --mount=type=ssh git submodule update --init --recursive
#RUN --mount=type=ssh ci/dynamic-deps.sh
#ENV LD_LIBRARY_PATH=/oak-compiler/external_rust_crypto_libs
#RUN stack build --copy-bins --ghc-options -j4

#FROM node:11 as node-build
WORKDIR /
RUN mkdir -p -m 0600 ~/.ssh && ssh-keyscan gitlab.com >> ~/.ssh/known_hosts
RUN --mount=type=ssh git clone git@gitlab.com:Concordium/node-dashboard.git
WORKDIR /node-dashboard
ENV NODE_ENV=development
# Building node dashboard
RUN npm i
RUN npm run build
# Node dashbaord built

FROM ubuntu:19.10
EXPOSE 8888
EXPOSE 10000
ENV RPC_SERVER_ADDR=0.0.0.0
ENV MODE=basic
ENV BOOTSTRAP_FIRST_NODE=bootstrap.eu.staging.concordium.com:8888
ENV DATA_DIR=/var/lib/concordium/data
ENV CONFIG_DIR=/var/lib/concordium/config
ENV EXTRA_ARGS="--no-dnssec"
ENV NODE_URL=localhost:10000
ENV COLLECTORD_URL=https://dashboard.eu.staging.concordium.com/nodes/post
ENV GRPC_HOST=http://localhost:10000
ENV DISTRIBUTION_CLIENT=true
RUN apt-get update && apt-get install -y unbound curl netbase ca-certificates supervisor nginx libtinfo6 libpq-dev liblmdb-dev jq
COPY --from=build /build-project/p2p_client-cli /p2p_client-cli
COPY --from=build /build-project/node-collector /node-collector
COPY --from=build /build-project/start.sh /start.sh
COPY --from=build /build-project/genesis.dat /genesis.dat
RUN sha256sum /genesis.dat
COPY --from=haskell-build /libs/* /usr/lib/
COPY --from=haskell-build /middleware /middleware
COPY --from=haskell-build /concordium-client-bin /usr/local/bin/concordium-client
COPY --from=haskell-build /genesis-binaries /genesis-binaries
COPY --from=haskell-build  /config-add-account.sh /usr/local/bin/config-add-account.sh
COPY --from=node-build /node-dashboard/dist/public /var/www/html/
#COPY --from=oak-build /oak-compiler/out/oak /usr/local/bin/oak
RUN mkdir /var/www/html/public
RUN mv /var/www/html/*.js /var/www/html/public/
RUN sed -i 's/try_files.*$/try_files \$uri \/index.html =404;/g' /etc/nginx/sites-available/default
RUN ln -s /usr/lib/x86_64-linux-gnu/libtinfo.so.6.1 /usr/lib/x86_64-linux-gnu/libtinfo.so.5
RUN chmod a+x /usr/local/bin/config-add-account.sh
COPY ./scripts/supervisord.conf /etc/supervisor/supervisord.conf
COPY ./scripts/concordium.conf /etc/supervisor/conf.d/concordium.conf
COPY ./scripts/staging-net-client.sh /staging-net-client.sh
ENTRYPOINT [ "/staging-net-client.sh" ]
