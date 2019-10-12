FROM ocurrent/opam@sha256:4d2dc158efda4a5920440e007638ff08dc2ad8c3af1d0a9ed777924ce6db94fa
#FROM ocurrent/opam:alpine-3.10-ocaml-4.08
RUN cd ~/opam-repository && git fetch && git reset --hard ca18b54339548dc814304558e87517e776016293 && opam update
RUN opam depext -i capnp afl-persistent conf-capnproto tls mirage-flow-lwt mirage-kv-lwt mirage-clock ptime cmdliner mirage-dns
ADD --chown=opam *.opam /home/opam/capnp-rpc/
WORKDIR /home/opam/capnp-rpc/
RUN opam install --deps-only -t .
ADD --chown=opam . /home/opam/capnp-rpc
RUN opam config exec -- make all test
