FROM ocaml/opam2@sha256:507d4e2786904e3d19ca6db6f6a482af131c9fe6b2597ba62add762305d5c0fb
#FROM ocaml/opam2:debian-9-ocaml-4.07
RUN sudo apt-get update
RUN git fetch && git reset --hard 7a2c776e4d7760fbc0a6bb500aaef20a6fe98e34 && opam update
ADD *.opam /home/opam/capnp-rpc/
WORKDIR /home/opam/capnp-rpc/
RUN opam pin add -ny capnp-rpc.dev . && \
    opam pin add -ny capnp-rpc-lwt.dev . && \
    opam pin add -ny capnp-rpc-unix.dev . && \
    opam pin add -ny capnp-rpc-mirage.dev . && \
    opam depext capnp-rpc-unix capnp-rpc-mirage
RUN opam install capnp-rpc-unix capnp-rpc-mirage alcotest-lwt afl-persistent io-page-unix tcpip mirage-vnetif
ADD . /home/opam/capnp-rpc
RUN sudo chown -R opam /home/opam/capnp-rpc
RUN opam config exec -- make all test
