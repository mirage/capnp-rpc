FROM ocaml/opam@sha256:5fbcf884db3539136a641a70c8c85f7314e09978568743ea2bb3377fc074a407
#FROM ocaml/opam:debian-9_ocaml-4.05.0
RUN cd opam-repository && git fetch && git reset --hard 56f11ae456e23db0ae58e561d2fd5582def984f9 && opam update
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
