FROM ocaml/opam@sha256:5e8ebc171a90fb62209e67dcaeedafd02018bc43ebc1e3074a5d03f9789f0ca1
#FROM ocaml/opam:debian-9_ocaml-4.05.0
RUN cd opam-repository && git fetch && git reset --hard 3cad9b6baa95451f294008d0b791c2b0d54b0968 && opam update
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
