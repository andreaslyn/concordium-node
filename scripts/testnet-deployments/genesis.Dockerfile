# syntax=docker/dockerfile:experimental
FROM alpine/git:latest as data
ARG genesis_ref
ARG genesis_path
RUN mkdir -p -m 0600 ~/.ssh && ssh-keyscan gitlab.com >> ~/.ssh/known_hosts
RUN --mount=type=ssh git clone --depth 1 --branch "${genesis_ref}" git@gitlab.com:Concordium/genesis-data.git /tmp/genesis-data
RUN mv /tmp/genesis-data/"${genesis_path}" /genesis-data && rm -rf /tmp/genesis-data

FROM alpine:3.13
COPY --from=data /genesis-data/genesis.dat /genesis.dat
COPY ./copy-genesis-dat.sh /copy-genesis-dat.sh
ENTRYPOINT ["/copy-genesis-dat.sh"]
