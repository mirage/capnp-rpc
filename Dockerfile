FROM ocaml/opam@sha256:a469435632d0cacbceab799d7e48201b727d025fa1805cbbe210d94233b251ad
#FROM ocaml/opam:debian-9_ocaml-4.04.0
ADD *.opam /home/opam/capnp-rpc/
WORKDIR /home/opam/capnp-rpc/
RUN opam pin add -ny capnp "https://github.com/talex5/capnp-ocaml.git#interfaces" && \
    opam pin add -ny capnp-rpc . && \
    opam pin add -ny capnp-rpc-lwt . && \
    opam depext capnp-rpc-lwt
RUN opam install capnp-rpc-lwt alcotest
ADD . /home/opam/capnp-rpc
RUN sudo chown -R opam /home/opam/capnp-rpc
RUN opam config exec -- make test
