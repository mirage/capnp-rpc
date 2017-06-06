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
    opam pin add -nyk git capnp-rpc-lwt .
    opam depext capnp-rpc-lwt alcotest
    opam install --deps-only -t capnp-rpc-lwt
    make test

The `examples` directory contains some test services.
Running `make test` will run through the tests in `test-lwt/test.ml`, which make use of the examples.

If you have trouble building, you can build it with Docker from a known-good state using `docker build .`.

### Structure of the library

The code is split into two libraries:

- `capnp-rpc` contains the logic of the [Cap'n Proto RPC Protocol][], but does not depend on any particular serialisation.
  The tests in the `test` directory test the logic using a simple representation where messages are OCaml data-structures
  (defined at `capnp-rpc/protocol.ml`'s `MessageTypes.t`).

- `capnp-rpc-lwt` instantiates the `capnp-rpc` functor using the Cap'n Proto serialisation for messages and Lwt for concurrency.
  The tests in `test-lwt` test this by sending Cap'n Proto messages over a Unix-domain socket.

Users of the library will normally want to use `capnp-rpc-lwt`.

### Conceptual model

An RPC system contains multiple communicating actors (just ordinary OCaml objects).
An actor can hold *capabilities* to other objects.
A capability here is just a regular OCaml object pointer.

Essentially, each object provides a `call` method, which takes:

- some pure-data message content (typically an array of bytes created by the Cap'n Proto serialisation), and
- an array of pointers to other objects (providing the same API).

The data part of the message says which method to invoke and provides the arguments.
Whenever an argument needs to refer to another object, it gives the index of a pointer in the pointers array.

For example, a call to a method that transfers data between two stores might look something like this:

```
- Content:
  - InterfaceID: xxx
  - MethodID: yyy
  - Params:
    - Source: 0
    - Target: 1
- Pointers:
  - <source>
  - <target>
```

A call returns a promise for the response message, which will also contain data and pointer parts.

This might seem like a rather clumsy representation, but it has the advantage that such messages can be sent not just within a process, like regular OCaml method calls, but also over the network to remote objects.

On top of this basic model the Cap'n Proto schema compiler ([capnp-ocaml]) generates a typed API, so that application code can only generate or attempt to consume messages that match the schema.
Application code does not need to worry about interface or method IDs, for example.

To set up an RPC system across multiple hosts, you must take an object as defined above and make it public, with:

```
let my_service = ... in
Capnp_rpc_lwt.CapTP.of_endpoint ~offer:my_service ~switch endpoint
```

When remote services establish a connection to `endpoint` (which is some kind of listening socket), they can ask for the `bootstrap` object and get back a capability reference to `my_service`.
This capability is a proxy object that acts like a local service but sends any messages over the network.
When a message is sent that contains pointers, the RPC system holds onto the pointers and makes each object available over that network connection.
Therefore, each host only needs to expose a single bootstrap object,
since the bootstrap object can provide methods to get access to any other required services.

The RPC system is smart enough that if you export an object to a remove service and it later exports the same object back to you, it will switch to sending directly to the local service (once any pipelined messages in flight have been delivered).

You should also be able to export an object that you received from a third-party, and the receiver should be able to establish a direct connection.
However, this isn't yet implemented and instead the RPC systems will forward messages and responses.

### Examples

TODO: Add some examples here once the API is stable.


[capnp-ocaml]: https://github.com/pelzlpj/capnp-ocaml
[Cap'n Proto]: https://capnproto.org/
[Cap'n Proto RPC Protocol]: https://capnproto.org/rpc.html
