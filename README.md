## OCaml Cap'n'Proto RPC library

Status: **experimental** and **unfinished**

Copyright 2017 Docker, Inc.
Released under the [Apache 2.0 license](LICENSE).

The calculator and RPC schema files are from the [Cap'n Proto][] project and have their own license (MIT;
included in the files).

### Overview

[Cap'n Proto][] is a capability-based RPC system with bindings for many languages.
Some key features:

- APIs are defined using a schema file, which is compiled to create bindings for different languages automatically.

- Schemas can be upgraded in many ways without breaking backwards-compatibility.

- Messages are built up and read in-place, making it very fast.

- Messages can contain *capability references*, allowing the sender to share access to a service. Access control is handled automatically.

- Messages can be *pipelined*. For example, you can ask one service where another one is, and then immediately start calling methods on it. The requests will be sent to the first service, which will either handle them (in the common case where the second service is in the same place) or forward them until you can establish a direct connection.

This library should be used with the [capnp-ocaml][] schema compiler, which generates bindings from schema files.

Currently, you need to pin the <https://github.com/talex5/capnp-ocaml/tree/interfaces> branch, which adds support for compiling interface definitions.


This library is new and unfinished. Check the issues page for some of the known bugs.


### Quick start

To build, you will need a platform with the capnproto package available (e.g. Debian >= 9). Then:

    git clone https://github.com/mirage/capnp-rpc.git
    cd capnp-rpc
    opam pin add -n capnp "https://github.com/talex5/capnp-ocaml.git#interfaces"
    opam pin add -nyk git capnp-rpc .
    opam depext capnp-rpc
    opam install --deps-only -t capnp-rpc
    make

The `examples` directory contains some test services.
Running `make` will run through the tests in `test/test.ml', which make use of the examples.

TODO: Add some examples here once the API is stable.


[capnp-ocaml]: https://github.com/pelzlpj/capnp-ocaml
[Cap'n Proto]: https://capnproto.org/
