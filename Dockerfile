FROM ocurrent/opam@sha256:7cec1ab422d97bf498c309a38b685e7c2650a0daa2d6ddef5fb4428de0535f26
#FROM ocurrent/opam:alpine-3.10-ocaml-4.08
RUN cd ~/opam-repository && git fetch && git reset --hard 42f3767f07a36517910bdbf5d0c5230457ae7c79 && opam update
RUN opam depext -i capnp afl-persistent conf-capnproto tls tls-mirage mirage-flow ptime cmdliner dns-client dns-mirage
ADD --chown=opam *.opam /home/opam/capnp-rpc/
WORKDIR /home/opam/capnp-rpc/
RUN opam pin add -yn capnp-rpc.dev . && \
    opam pin add -yn capnp-rpc-lwt.dev . && \
    opam pin add -yn capnp-rpc-net.dev . && \
    opam pin add -yn capnp-rpc-unix.dev . && \
    opam pin add -yn capnp-rpc-mirage.dev . 
RUN opam install --deps-only -t .
ADD --chown=opam . /home/opam/capnp-rpc
RUN opam exec -- make all test
