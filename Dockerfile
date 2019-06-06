FROM ocaml/opam2@sha256:d76c296ff72d775c47b089555eca67b2a1d737498e4e5fdb8b73b6ba0630a1a4
#FROM ocaml/opam2:debian-9-ocaml-4.07
RUN sudo apt-get update
RUN git fetch && git reset --hard 15fe01bbd69385c8103e22a60f1087d21068cc16 && opam update
RUN opam depext -i capnp afl-persistent conf-capnproto tls mirage-flow-lwt mirage-kv-lwt mirage-clock ptime cmdliner mirage-dns
ADD --chown=opam *.opam /home/opam/capnp-rpc/
WORKDIR /home/opam/capnp-rpc/
RUN opam install --deps-only -t .
ADD --chown=opam . /home/opam/capnp-rpc
RUN opam config exec -- make all test
