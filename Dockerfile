FROM ocaml/opam:debian-11-ocaml-4.14@sha256:5b9de826b22c77a0654519d0959536f93a6ffd7020712a8b1c3437445e031e04 AS build
RUN sudo apt-get update && sudo apt-get install libev-dev capnproto m4 pkg-config libsqlite3-dev libgmp-dev -y --no-install-recommends
RUN cd ~/opam-repository && git pull origin -q master && git reset --hard 97da9a1b68b824a65a09e5f7d071fcf2da35bd1b && opam update
COPY --chown=opam ocluster-api.opam ocluster.opam /src/
COPY --chown=opam obuilder/obuilder.opam obuilder/obuilder-spec.opam /src/obuilder/
RUN opam pin -yn /src/obuilder/
WORKDIR /src
RUN opam install -y --deps-only .
ADD --chown=opam . .
RUN opam exec -- dune subst
RUN opam config exec -- dune build \
  ./_build/install/default/bin/ocluster-scheduler \
  ./_build/install/default/bin/ocluster-admin

FROM debian:11
RUN apt-get update && apt-get install libev4 libsqlite3-0 -y --no-install-recommends
RUN apt-get install ca-certificates -y  # https://github.com/mirage/ocaml-conduit/issues/388
WORKDIR /var/lib/ocluster-scheduler
ENTRYPOINT ["/usr/local/bin/ocluster-scheduler"]
COPY --from=build \
     /src/_build/install/default/bin/ocluster-scheduler \
     /src/_build/install/default/bin/ocluster-admin \
     /usr/local/bin/
