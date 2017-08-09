FROM ocaml/opam@sha256:374934a4a512f1adf96f0a2858ef68d27719af96b7d0c28e5206f207edd98b6d
#FROM ocaml/opam:debian-9_ocaml-4.04.0
RUN cd opam-repository && git fetch && git reset --hard d991b129d7e5834a73ee8d45e700955ecbc2e444 && opam update
ADD *.opam /home/opam/capnp-rpc/
WORKDIR /home/opam/capnp-rpc/
RUN opam pin add -ny capnp-rpc . && \
    opam pin add -ny capnp-rpc-lwt . && \
    opam pin add -ny capnp-rpc-unix . && \
    opam depext capnp-rpc-unix
RUN opam install capnp-rpc-unix alcotest afl-persistent
ADD . /home/opam/capnp-rpc
RUN sudo chown -R opam /home/opam/capnp-rpc
RUN opam config exec -- make all test
