FROM ocaml/opam2@sha256:95a87466c507b160bb18e696951b55fe0ace0bdc1fe7f16cbb827ae74ef74198
#FROM ocaml/opam2:debian-10-ocaml-4.08
RUN sudo apt-get update
RUN git fetch && git reset --hard 22aca1413d66271baf995f79feecf7430fc988f1 && opam update
RUN opam depext -i capnp afl-persistent conf-capnproto tls mirage-flow-lwt mirage-kv-lwt mirage-clock ptime cmdliner mirage-dns
ADD --chown=opam *.opam /home/opam/capnp-rpc/
WORKDIR /home/opam/capnp-rpc/
RUN opam install --deps-only -t .
ADD --chown=opam . /home/opam/capnp-rpc
RUN opam config exec -- make all test
