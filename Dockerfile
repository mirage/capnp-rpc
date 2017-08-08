FROM ocaml/opam@sha256:374934a4a512f1adf96f0a2858ef68d27719af96b7d0c28e5206f207edd98b6d
#FROM ocaml/opam:debian-9_ocaml-4.04.0
RUN cd opam-repository && git fetch && git reset --hard 109564a1fa93e39ef9a41582104718153a6e3abe && opam update
ADD *.opam /home/opam/capnp-rpc/
WORKDIR /home/opam/capnp-rpc/
RUN opam pin add -ny capnp "https://github.com/talex5/capnp-ocaml.git#interfaces6" && \
    opam pin add -ny capnp-rpc . && \
    opam pin add -ny capnp-rpc-lwt . && \
    opam pin add -ny capnp-rpc-unix . && \
    opam depext capnp-rpc-unix
RUN opam install capnp-rpc-unix alcotest afl-persistent
ADD . /home/opam/capnp-rpc
RUN sudo chown -R opam /home/opam/capnp-rpc
RUN opam config exec -- make all test
