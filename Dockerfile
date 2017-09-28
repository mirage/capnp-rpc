FROM ocaml/opam@sha256:683543a56e30160a82778f1d2e795f23579a71e5b5554a4d36b2b44421629cdd
#FROM ocaml/opam:debian-9_ocaml-4.05.0
RUN cd opam-repository && git fetch && git reset --hard 5076ad3874e50af8cc47a4fb5c49b3f590010b76 && opam update
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
