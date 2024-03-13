# OCaml Cap'n Proto RPC library

Copyright 2017 Docker, Inc.
Copyright 2022 Thomas Leonard.
See [LICENSE.md](LICENSE.md) for details.

[API documentation][api]

## Contents

<!-- vim-markdown-toc GFM -->

* [Overview](#overview)
* [Status](#status)
* [Installing](#installing)
* [Structure of the library](#structure-of-the-library)
* [Tutorial](#tutorial)
  * [A basic echo service](#a-basic-echo-service)
  * [Passing capabilities](#passing-capabilities)
  * [Networking](#networking)
    * [The server side](#the-server-side)
    * [The client side](#the-client-side)
    * [Separate processes](#separate-processes)
* [Pipelining](#pipelining)
* [Guide to sturdy refs](#guide-to-sturdy-refs)
  * [Hosting multiple sturdy refs](#hosting-multiple-sturdy-refs)
  * [Creating services dynamically](#creating-services-dynamically)
  * [The Persistence API](#the-persistence-api)
  * [Persisting sturdy refs](#persisting-sturdy-refs)
* [Further reading](#further-reading)
* [FAQ](#faq)
  * [Why does my connection stop working after 10 minutes?](#why-does-my-connection-stop-working-after-10-minutes)
  * [How can I return multiple results?](#how-can-i-return-multiple-results)
  * [Can I create multiple instances of an interface dynamically?](#can-i-create-multiple-instances-of-an-interface-dynamically)
  * [Can I get debug output?](#can-i-get-debug-output)
  * [How can I debug reference counting problems?](#how-can-i-debug-reference-counting-problems)
  * [How can I import a sturdy ref that I need to start my vat?](#how-can-i-import-a-sturdy-ref-that-i-need-to-start-my-vat)
  * [How can I release other resources when my service is released?](#how-can-i-release-other-resources-when-my-service-is-released)
  * [Is there an interactive version I can use for debugging?](#is-there-an-interactive-version-i-can-use-for-debugging)
  * [Can I set up a direct 2-party connection over a pre-existing channel?](#can-i-set-up-a-direct-2-party-connection-over-a-pre-existing-channel)
* [Contributing](#contributing)
  * [Conceptual model](#conceptual-model)
  * [Building](#building)
  * [Testing](#testing)
  * [Fuzzing](#fuzzing)

<!-- vim-markdown-toc -->

## Overview

[Cap'n Proto][] is a capability-based RPC system with bindings for many languages.
Some key features:

- APIs are defined using a schema file, which is compiled to create bindings for different languages automatically.

- Schemas can be upgraded in many ways without breaking backwards-compatibility.

- Messages are built up and read in-place, making it very fast.

- Messages can contain *capability references*, allowing the sender to share access to a service. Access control is handled automatically.

- Messages can be *pipelined*. For example, you can ask one service where another one is, and then immediately start calling methods on it. The requests will be sent to the first service, which will either handle them (in the common case where the second service is in the same place) or forward them until you can establish a direct connection.

- Messages are delivered in [E-Order][], which means that messages sent over a reference will arrive in the order in which they were sent, even if the path they take through the network gets optimised at some point.

This library should be used with the [capnp-ocaml][] schema compiler, which generates bindings from schema files.


## Status

RPC Level 2 is complete, with encryption and authentication using TLS and support for persistence.

The library has unit tests and AFL fuzz tests that cover most of the core logic.
It is used as the RPC system in [ocaml-ci][].

The default network provided supports TCP and Unix-domain sockets, both with or without TLS.
For two-party networking, you can provide any bi-directional byte stream (satisfying the Mirage flow signature)
to the library to create a connection.
You can also define your own network types.

Level 3 support is not implemented yet, so if host Alice has connections to hosts Bob and Carol and passes an object hosted at Bob to Carol, the resulting messages between Carol and Bob will be routed via Alice.
Until that is implemented, Carol can ask Bob for a persistent reference (sturdy ref) and then connect directly to that.


## Installing

To install, you will need a platform with the capnproto package available (e.g. Debian >= 9). Then:

    opam depext -i capnp-rpc-unix

## Structure of the library

The code is split into several packages:

- `capnp-rpc` contains the logic of the [Cap'n Proto RPC Protocol][], but does not depend on any particular serialisation.
  The tests in the `test` directory test the logic using a simple representation where messages are OCaml data-structures
  (defined in `capnp-rpc/message_types.ml`).

- `capnp-rpc-lwt` instantiates the `capnp-rpc` functor using the Cap'n Proto serialisation for messages and Lwt for concurrency.

- `capnp-rpc-net` adds networking support, including TLS.

- `capnp-rpc-unix` adds helper functions for parsing command-line arguments and setting up connections over Unix sockets.
  The tests in `test-lwt` test this by sending Cap'n Proto messages over a Unix-domain socket.

- `capnp-rpc-mirage` is an alternative to `-unix` that works with [Mirage][] unikernels.

**Libraries** that consume or provide Cap'n Proto services should normally depend only on `capnp-rpc-lwt`,
since they shouldn't care whether the services they use are local or accessed over some kind of network.

**Applications** will normally want to use `capnp-rpc-net` and, in most cases, `capnp-rpc-unix`.

## Tutorial

This tutorial creates a simple echo service and then extends it.
It shows how to use most of the features of the library, including defining services,
sending and receiving messages, and using encryption and authentication over network links.

When following the tutorial, you should be able to create all the files yourself, following the instructions.
If you get stuck, you can find complete solutions in the [examples](./examples/) directory.

### A basic echo service

We start by writing a [Cap'n Proto schema file][schema]:

```capnp
interface Echo {
  ping @0 (msg :Text) -> (reply :Text);
}
```

This defines the `Echo` interface as having a single method called `ping`
which takes a struct containing a text field called `msg`
and returns a struct containing another text field called `reply`.

Save this as `echo_api.capnp` and compile it using capnp:

<!-- $MDX skip -->
```sh
$ capnp compile echo_api.capnp -o ocaml
echo_api.capnp:1:1: error: File does not declare an ID.  I've generated one for you.
Add this line to your file:
@0xb287252b6cbed46e;
```

Every interface needs a globally unique ID.
If you don't have one, capnp will pick one for you, as shown above.
Add the line to the start of the file to get:

<!-- $MDX include,file=examples/v1/echo_api.capnp -->
```capnp
@0xb287252b6cbed46e;

interface Echo {
  ping @0 (msg :Text) -> (reply :Text);
}
```

Now it can be compiled:

<!-- $MDX dir=examples/v1 -->
```sh
$ capnp compile echo_api.capnp -o ocaml
echo_api.capnp --> echo_api.mli echo_api.ml
```

The next step is to implement a client and server (in a new `echo.ml` file) using the generated `Echo_api` OCaml module.

For the server, you should inherit from the generated `Api.Service.Echo.service` class:

<!-- $MDX include,file=examples/v1/echo.ml,part=server -->
```ocaml
module Api = Echo_api.MakeRPC(Capnp_rpc_lwt)

open Capnp_rpc_lwt

let local =
  let module Echo = Api.Service.Echo in
  Echo.local @@ object
    inherit Echo.service

    method ping_impl params release_param_caps =
      let open Echo.Ping in
      let msg = Params.msg_get params in
      release_param_caps ();
      let response, results = Service.Response.create Results.init_pointer in
      Results.reply_set results ("echo:" ^ msg);
      Service.return response
  end
```

The first line (`module Api`) instantiates the generated code to use this library's RPC implementation.

The service object must provide one OCaml method for each method defined in the schema file, with `_impl` on the end of each one.

There's a bit of ugly boilerplate here, but it's quite simple:

- The `Api.Service.Echo.Ping` module defines the server-side API for the `ping` method.
- `Ping.Params` is a reader for the parameters.
- `Ping.Results` is a builder for the results.
- `msg` is the string value of the `msg` field.
- `release_param_caps` releases any capabilities passed in the parameters.
  In this case there aren't any, but remember that a client using some future
  version of this protocol might pass some optional capabilities, and so you
  should always free them anyway.
- `Service.Response.create Results.init_pointer` creates a new response message, using `Ping.Results.init_pointer` to initialise the payload contents.
- `response` is the complete message to be sent back, and `results` is the data part of it.
- `Service.return` returns the results immediately (rather than returning a promise).

The client implementation is similar, but uses `Api.Client` instead of `Api.Service`.
Here, we have a *builder* for the parameters and a *reader* for the results.
`Api.Client.Echo.Ping.method_id` is a globally unique identifier for the ping method.

<!-- $MDX include,file=examples/v1/echo.ml,part=client -->
```ocaml
module Echo = Api.Client.Echo

let ping t msg =
  let open Echo.Ping in
  let request, params = Capability.Request.create Params.init_pointer in
  Params.msg_set params msg;
  Capability.call_for_value_exn t method_id request |> Results.reply_get
```

`Capability.call_for_value_exn` sends the request message to the service and waits for the response to arrive.
If the response is an error, it raises an exception.
`Results.reply_get` extracts the `reply` field of the result.

We don't need to release the capabilities of the results, as `call_for_value_exn` does that automatically.
We'll see how to handle capabilities later.

With the boilerplate out of the way, we can now write a `main.ml` to test it:

<!-- $MDX include,file=examples/v1/main.ml -->
```ocaml
open Eio.Std

let () =
  Logs.set_level (Some Logs.Warning);
  Logs.set_reporter (Logs_fmt.reporter ())

let () =
  Eio_main.run @@ fun _ ->
  let service = Echo.local in
  let reply = Echo.ping service "foo" in
  traceln "Got reply %S" reply
```

<p align='center'>
  <img src="./diagrams/ping.svg"/>
</p>

Here's a suitable `dune` file to compile the schema file and then the generated OCaml files:

<!-- $MDX include,file=examples/v1/dune -->
```
(executable
 (name main)
 (libraries eio_main capnp-rpc-lwt logs.fmt)
 (flags (:standard -w -53-55)))

(rule
 (targets echo_api.ml echo_api.mli)
 (deps    echo_api.capnp)
 (action (run capnp compile -o %{bin:capnpc-ocaml} %{deps})))
```

With this, you can now delete the generated files from your source directory:

<!-- $MDX dir=examples/v1 -->
```sh
$ rm echo_api.ml echo_api.mli
```

The service is now usable:

<!-- $MDX skip -->
```bash
$ opam depext -i capnp-rpc-lwt
```

<!-- $MDX dir=examples/v1 -->
```bash
$ dune exec ./main.exe
+Got reply "echo:foo"
```

This isn't very exciting, so let's add some capabilities to the protocol...

### Passing capabilities

Let's update the schema:

<!-- $MDX include,file=examples/v2/echo_api.capnp -->
```capnp
@0xb287252b6cbed46e;

interface Callback {
  log @0 (msg :Text) -> ();
}

interface Echo {
  ping      @0 (msg :Text) -> (reply :Text);
  heartbeat @1 (msg :Text, callback :Callback) -> ();
}
```

This version of the protocol adds a `heartbeat` method.
Instead of returning the text directly, it will send it to a callback at regular intervals.

The new `heartbeat_impl` method looks like this:

<!-- $MDX include,file=examples/v2/echo.ml,part=server-heartbeat -->
```ocaml
    method heartbeat_impl params release_params =
      let open Echo.Heartbeat in
      let msg = Params.msg_get params in
      let callback = Params.callback_get params in
      release_params ();
      match callback with
      | None -> Service.fail "No callback parameter!"
      | Some callback ->
        Capability.with_ref callback (notify ~clock msg)
```

Note that all parameters in Cap'n Proto are optional, so we have to check for `callback` not being set
(data parameters such as `msg` get a default value from the schema, which is
`""` for strings if not set explicitly).

`Capability.with_ref x f` calls `f x` and then releases `x` (capabilities are ref-counted).

`notify ~clock msg callback` just sends a few messages to `callback` in a loop:

<!-- $MDX include,file=examples/v2/echo.ml,part=notify -->
```ocaml
let notify ~clock msg callback =
  let rec loop = function
    | 0 ->
      Service.return_empty ()
    | i ->
      match Callback.log callback msg with
      | Error (`Capnp e) -> Service.error e
      | Ok () ->
        Eio.Time.sleep clock 1.0;
        loop (i - 1)
  in
  loop 3
```

Exercise: create a `Callback` submodule in `echo.ml` and implement the client-side `Callback.log` function (hint: it's very similar to `ping`, but use `Capability.call_for_unit` because we don't care about the value of the result and we want to handle errors manually). If you get stuck, the solution can be found in the `examples/v2` directory.

To write the client for `Echo.heartbeat`, we take a user-provided callback object
and put it into the request:

<!-- $MDX include,file=examples/v2/echo.ml,part=client-heartbeat -->
```ocaml
let heartbeat t msg callback =
  let open Echo.Heartbeat in
  let request, params = Capability.Request.create Params.init_pointer in
  Params.msg_set params msg;
  Params.callback_set params (Some callback);
  Capability.call_for_unit_exn t method_id request
```

`Capability.call_for_unit_exn` is a convenience wrapper around
`Callback.call_for_value_exn` that discards the result.

In `main.ml`, we can now wrap a regular OCaml function as the callback:

<!-- $MDX include,file=examples/v2/main.ml -->
```ocaml
open Eio.Std
open Capnp_rpc_lwt

let () =
  Logs.set_level (Some Logs.Warning);
  Logs.set_reporter (Logs_fmt.reporter ())

let callback_fn msg =
  traceln "Callback got %S" msg

let run_client service =
  Capability.with_ref (Echo.Callback.local callback_fn) @@ fun callback ->
  Echo.heartbeat service "foo" callback

let () =
  Eio_main.run @@ fun env ->
  let clock = if Sys.getenv_opt "CI" = None then env#clock else Fake_clock.v in
  let service = Echo.local ~clock in
  run_client service
```

Step 1: The client creates the callback:

<p align='center'>
  <img src="./diagrams/callback1.svg"/>
</p>

Step 2: The client calls the `heartbeat` method, passing the callback as an argument:

<p align='center'>
  <img src="./diagrams/callback2.svg"/>
</p>

Step 3: The service receives the callback and calls the `log` method on it:

<p align='center'>
  <img src="./diagrams/callback3.svg"/>
</p>


Exercise: implement `Callback.local fn` (hint: it's similar to the original `ping` service, but pass the message to `fn` and return with `Service.return_empty ()`)

And testing it should give (three times, at one second intervals):

<!-- $MDX dir=examples/v2,set-CI=true -->
```sh
$ dune exec -- ./main.exe
+Callback got "foo"
+Callback got "foo"
+Callback got "foo"
```

Note that the client gives the echo service permission to call its callback service by sending a message containing the callback to the service.
No other access control updates are needed.

Note also a design choice here in the API: we could have made the `Echo.heartbeat` function take an OCaml callback and wrap it, but instead we chose to take a service and make `main.ml` do the wrapping.
The advantage to doing it this way is that `main.ml` may one day want to pass a remote callback, as we'll see later.

This still isn't very exciting, because we just stored an OCaml object pointer in a message and then pulled it out again.
However, we can use the same code with the echo client and service in separate processes, communicating over the network...

### Networking

Let's put a network connection between the client and the server.
Here's the new `main.ml` (the top half is the same as before):

<!-- $MDX include,file=examples/v3/main.ml -->
```ocaml
open Eio.Std
open Capnp_rpc_lwt

let () =
  Logs.set_level (Some Logs.Warning);
  Logs.set_reporter (Logs_fmt.reporter ())

let callback_fn msg =
  traceln "Callback got %S" msg

let run_client service =
  Capability.with_ref (Echo.Callback.local callback_fn) @@ fun callback ->
  Echo.heartbeat service "foo" callback

let secret_key = `Ephemeral
let listen_address = `TCP ("127.0.0.1", 7000)

let start_server ~sw ~clock net =
  let config = Capnp_rpc_unix.Vat_config.create ~secret_key listen_address in
  let service_id = Capnp_rpc_unix.Vat_config.derived_id config "main" in
  let restore = Capnp_rpc_net.Restorer.single service_id (Echo.local ~clock) in
  let vat = Capnp_rpc_unix.serve ~sw ~net ~restore config in
  Capnp_rpc_unix.Vat.sturdy_uri vat service_id

let () =
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  let clock = if Sys.getenv_opt "CI" = None then env#clock else Fake_clock.v in
  let uri = start_server ~sw ~clock env#net in
  traceln "Connecting to echo service at: %a" Uri.pp_hum uri;
  let client_vat = Capnp_rpc_unix.client_only_vat ~sw env#net in
  let sr = Capnp_rpc_unix.Vat.import_exn client_vat uri in
  Sturdy_ref.with_cap_exn sr run_client;
  raise Exit
```

<p align='center'>
  <img src="./diagrams/vats.svg"/>
</p>

You'll need to edit your `dune` file to add a dependency on `capnp-rpc-unix` in the `(libraries ...` line and also:

<!-- $MDX skip -->
```sh
$ opam depext -i capnp-rpc-unix
```

Running this will give something like:

<!-- $MDX dir=examples/v3,non-deterministic,set-CI=true -->
```sh
$ dune exec ./main.exe
Connecting to echo service at: capnp://sha-256:3Tj5y5Q2qpqN3Sbh0GRPxgORZw98_NtrU2nLI0-Tn6g@127.0.0.1:7000/eBIndzZyoVDxaJdZ8uh_xBx5V1lfXWTJCDX-qEkgNZ4
Callback got "foo"
Callback got "foo"
Callback got "foo"
```

Once the server vat is running, we get a "sturdy ref" for the echo service, which is displayed as a "capnp://" URL.
The URL contains several pieces of information:

- The `sha-256:3Tj5y5Q2qpqN3Sbh0GRPxgORZw98_NtrU2nLI0-Tn6g` part is the fingerprint of the server's public key.
  When the client connects, it uses this to verify that it is connected to the right server (not an imposter).
  Therefore, a Cap'n Proto vat does not need to be certified by a CA (and cannot be compromised by a rogue CA).

- `127.0.0.1:7000` is the address to which clients will try to connect to reach the server vat.

- `eBIndzZyoVDxaJdZ8uh_xBx5V1lfXWTJCDX-qEkgNZ4` is the (base64-encoded) service ID.
  This is a secret that both identifies the service to use within the vat, and also grants access to it.

#### The server side

The ``let secret_key = `Ephemeral`` line causes a new server key to be generated each time the program runs,
so if you run it again you'll see a different capnp URL.
For a real system you'll want to save the key so that the server's identity doesn't change when it is restarted.
You can use ``let secret_key = `File "secret-key.pem"`` for that.
Then the file `secret-key.pem` will be created automatically the first time you start the service,
and reused on future runs.

It is also possible to disable the use of encryption using `Vat_config.create ~serve_tls:false ...`.
That might be useful if you need to interoperate with a client that doesn't support TLS.

`listen_address` tells the server where to listen for incoming connections.
You can use `` `Unix path`` for a Unix-domain socket at `path`, or
`` `TCP (host, port)`` to accept connections over TCP.

For TCP, you might want to listen on one address but advertise a different one, e.g.

<!-- $MDX skip -->
```ocaml
let listen_address = `TCP ("0.0.0.0", 7000)	(* Listen on all interfaces *)
let public_address = `TCP ("192.168.1.3", 7000)	(* Tell clients to connect here *)

let start_server () =
  let config = Capnp_rpc_unix.Vat_config.create ~secret_key ~public_address listen_address in
```

In `start_server`:

- `let service_id = Capnp_rpc_unix.Vat_config.derived_id config "main"` creates the secret ID that
  grants access to the service. `derived_id` generates the ID deterministically from the secret key
  and the name. This means that the ID will be stable as long as the server's key doesn't change.
  The name used ("main" here) isn't important - it just needs to be unique.

- `let restore = Capnp_rpc_net.Restorer.single service_id (Echo.local ~clock)`
  configures a simple "restorer" that answers requests for `service_id` with
  our `Echo.local` service.

- `Capnp_rpc_unix.serve config ~restore` creates the service vat using the
  previous configuration items and starts it listening for incoming connections.

- `Capnp_rpc_unix.Vat.sturdy_uri vat service_id` returns a "capnp://" URI for
  the given service within the vat.

#### The client side

After starting the server and getting the sturdy URI, we create a client vat and connect to the sturdy ref.
The result is a proxy to the remote service via the network that can be used in
exactly the same way as the direct reference we used before.

#### Separate processes

The example above runs the client and server in a single process, which is convenient for testing.
To run them in separate processes we just need to split `main.ml` into separate files
and add some command-line parsing to let the user pass the URL.

Edit the `dune` file to build a client and server:

<!-- $MDX include,file=examples/v4/dune -->
```
(executables
 (names client server)
 (libraries eio_main capnp-rpc-lwt logs.fmt capnp-rpc-unix)
 (flags (:standard -w -53-55)))

(rule
 (targets echo_api.ml echo_api.mli)
 (deps    echo_api.capnp)
 (action (run capnp compile -o %{bin:capnpc-ocaml} %{deps})))
```

Here's a suitable `server.ml`:

<!-- $MDX include,file=examples/v4/server.ml -->
```ocaml
open Eio.Std
open Capnp_rpc_net

let () =
  Logs.set_level (Some Logs.Warning);
  Logs.set_reporter (Logs_fmt.reporter ())

let cap_file = "echo.cap"

let serve config =
  Eio_main.run @@ fun env ->
  let clock = if Sys.getenv_opt "CI" = None then env#clock else Fake_clock.v in
  Switch.run @@ fun sw ->
  let service_id = Capnp_rpc_unix.Vat_config.derived_id config "main" in
  let restore = Restorer.single service_id (Echo.local ~clock) in
  let vat = Capnp_rpc_unix.serve ~sw ~net:env#net ~restore config in
  match Capnp_rpc_unix.Cap_file.save_service vat service_id cap_file with
  | Error `Msg m -> failwith m
  | Ok () ->
    traceln "Server running. Connect using %S." cap_file;
    Fiber.await_cancel ()

open Cmdliner

let serve_cmd =
  let doc = "run the server" in
  let info = Cmd.info "serve" ~doc in
  Cmd.v info Term.(const serve $ Capnp_rpc_unix.Vat_config.cmd)

let () =
  exit (Cmd.eval serve_cmd)
```

The cmdliner term `Capnp_rpc_unix.Vat_config.cmd` provides an easy way to get a suitable `Vat_config`
based on command-line arguments provided by the user.

And here's the corresponding `client.ml`:

<!-- $MDX include,file=examples/v4/client.ml -->
```ocaml
open Eio.Std
open Capnp_rpc_lwt

let () =
  Logs.set_level (Some Logs.Warning);
  Logs.set_reporter (Logs_fmt.reporter ())

let callback_fn msg =
  traceln "Callback got %S" msg

let run_client service =
  Capability.with_ref (Echo.Callback.local callback_fn) @@ fun callback ->
  Echo.heartbeat service "foo" callback

let connect uri =
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  let client_vat = Capnp_rpc_unix.client_only_vat ~sw env#net in
  let sr = Capnp_rpc_unix.Vat.import_exn client_vat uri in
  Capnp_rpc_unix.with_cap_exn sr run_client

open Cmdliner

let connect_addr =
  let i = Arg.info [] ~docv:"ADDR" ~doc:"Address of server (capnp://...)" in
  Arg.(required @@ pos 0 (some Capnp_rpc_unix.sturdy_uri) None i)

let connect_cmd =
  let doc = "run the client" in
  let info = Cmd.info "connect" ~doc in
  Cmd.v info Term.(const connect $ connect_addr)

let () =
  exit (Cmd.eval connect_cmd)
```

To test, start the server running:

<!-- $MDX skip -->
```sh
$ dune exec -- ./server.exe \
    --capnp-secret-key-file key.pem \
    --capnp-listen-address tcp:localhost:7000
Server running. Connect using "echo.cap".
```

With the server still running in another window, run the client using the `echo.cap` file generated by the server:

<!-- $MDX skip -->
```sh
$ dune exec ./client.exe echo.cap
Callback got "foo"
Callback got "foo"
Callback got "foo"
```

Note that we're using `Capnp_rpc_unix.with_cap_exn` here instead of `Sturdy_ref.with_cap_exn`.
It's almost the same, except that it displays a suitable progress indicator if the connection takes too long.

Congratulations on finishing the main tutorial! You now know how to:

- Define Cap'n Proto services and clients, independently of any networking.
- Pass capability references in method arguments and results.
- Stretch capabilities over a network link, with encryption, authentication and access control.
- Configure a vat using command-line arguments.

## Pipelining

Imagine we have a server with the following API:

<!-- $MDX include,file=examples/pipelining/echo_api.capnp -->
```capnp
@0xb287252b6cbed46e;

interface Callback {
  log @0 (msg :Text) -> ();
}

interface Echo {
  ping      @0 (msg :Text) -> (reply :Text);
  heartbeat @1 (msg :Text, callback :Callback) -> ();
  getLogger @2 () -> (callback :Callback);
}
```

It's the same API we saw in the tutorial above, but extended with a logging service,
which the client can get from the main echo service by calling `getLogger`.

The implementation of the new method in the service is simple -
we export the callback in the response in the same way we previously exported the client's callback in the request:

<!-- $MDX include,file=examples/pipelining/echo.ml,part=server-get-logger -->
```ocaml
    method get_logger_impl _ release_params =
      let open Echo.GetLogger in
      release_params ();
      let response, results = Service.Response.create Results.init_pointer in
      Results.callback_set results (Some service_logger);
      Service.return response
```

Exercise: create a `service_logger` that prints out whatever it gets (hint: use `Callback.local`)
See [examples/pipelining](./examples/pipelining/) for the solution.

The client side is more interesting:

<!-- $MDX include,file=examples/pipelining/echo.ml,part=client-get-logger -->
```ocaml
let get_logger t =
  let open Echo.GetLogger in
  let request = Capability.Request.create_no_args () in
  Capability.call_for_caps t method_id request Results.callback_get_pipelined
```

We could have used `call_and_wait` here
(which is similar to `call_for_value` but doesn't automatically discard any capabilities in the result).
However, that would mean waiting for the response to be sent back to us over the network before we could use it.
Instead, we use `callback_get_pipelined` to get a promise for the capability from the promise of the `getLogger` call's result.

Note: the last argument to `call_for_caps` is a function for extracting the capabilities from the promised result.
In the common case where you just want one and it's in the root result struct, you can just pass the accessor directly,
as shown.
Doing it this way allows `call_for_caps` to release any unused capabilities in the result automatically for us.

We can test it as follows:

<!-- $MDX include,file=examples/pipelining/main.ml,part=run-client -->
```ocaml
let run_client service =
  let logger = Echo.get_logger service in
  match Echo.Callback.log logger "Message from client" with
  | Ok () -> ()
  | Error (`Capnp err) ->
    Fmt.epr "Server's logger failed: %a" Capnp_rpc.Error.pp err
```

<p align='center'>
  <img src="./diagrams/pipeline.svg"/>
</p>

This should print (in the server's output) something like:

<!-- $MDX dir=examples/pipelining -->
```sh
$ dune exec ./main.exe
+[client] Connecting to echo service...
+[server] Received "Message from client"
Fatal error: exception Stdlib.Exit
[2]
```

In this case, we didn't wait for the `getLogger` call to return before using the logger.
The RPC library pipelined the `log` call directly to the promised logger from its previous question.
On the wire, the messages are sent together, and look like:

1. What is your logger?
2. Please call the object returned in answer to my previous question (1).

Now, let's say we'd like the server to send heartbeats to itself:

<!-- $MDX skip -->
```ocaml
let run_client service =
  Capability.with_ref (Echo.get_logger service) @@ fun callback ->
  Echo.heartbeat service "foo" callback
```

Here, we ask the server for its logger and then (without waiting for the reply), tell it to send heartbeat messages to the promised logger (you should see the messages appear in the server's output).

Previously, when we exported our local `callback` object, it arrived at the service as a proxy that sent messages back to the client over the network.
But when we send the (promise of the) server's own logger back to it, the RPC system detects this and "shortens" the path;
the capability reference that the `heartbeat` handler gets is a direct reference to its own logger, which
it can call without using the network.

These optimisations are very important because they allow us to build APIs like this with small functions that can be composed easily.
Without pipelining, we would be tempted to clutter the protocol with specialised methods like `heartbeatToYourself` to avoid the extra round-trips most RPC protocols would otherwise require.

## Guide to sturdy refs

A `Capability.t` is a "live" reference to a service, and is typically tied to a TCP connection.
If the TCP connection fails, or the client or server is restarted, the capability becomes broken and can no longer be used.
By contrast, a "sturdy ref" is an offline reference to a service, which can be saved as a URL in a file, emailed to someone, etc.
A sturdy ref can be used to get a live ref when needed.

### Hosting multiple sturdy refs

The `Restorer.single` restorer used in the tutorial above is useful for vats hosting a single sturdy ref.
However, you may want to host multiple sturdy refs,
perhaps to provide separate "admin" and "user" capabilities to different clients,
or to allow services to be created and persisted as sturdy refs dynamically.
To do this, we can use `Restorer.Table`.

[examples/sturdy-refs](./examples/sturdy-refs) is an example of this.
It defines a simple API for a logging service:

<!-- $MDX include,file=examples/sturdy-refs/api.capnp -->
```capnp
@0x9ab198a53301be6e;

interface Logger {
  log @0 (msg :Text) -> ();
}
```

Here is an example using it.
The server writes out two cap files, for two different logger instances.
Two different clients load these two files and send messages to the server:

<!-- $MDX include,file=examples/sturdy-refs/main.ml,part=main -->
```ocaml
let make_service ~config ~services name =
  let service = Logger.local name in
  let id = Capnp_rpc_unix.Vat_config.derived_id config name in
  Restorer.Table.add services id service;
  name, id

let start_server ~sw net =
  let config = Capnp_rpc_unix.Vat_config.create ~secret_key listen_address in
  let make_sturdy = Capnp_rpc_unix.Vat_config.sturdy_uri config in
  let services = Restorer.Table.create make_sturdy in
  let restore = Restorer.of_table services in
  let services = List.map (make_service ~config ~services) ["alice"; "bob"] in
  let vat = Capnp_rpc_unix.serve ~sw ~net ~restore config in
  services |> List.iter (fun (name, id) ->
      let cap_file = name ^ ".cap" in
      Capnp_rpc_unix.Cap_file.save_service vat id cap_file |> or_fail;
      Printf.printf "[server] saved %S\n%!" cap_file
    )

let run_client ~sw ~net cap_file msg =
  let vat = Capnp_rpc_unix.client_only_vat ~sw net in
  let sr = Capnp_rpc_unix.Cap_file.load vat cap_file |> or_fail in
  Printf.printf "[client] loaded %S\n%!" cap_file;
  Sturdy_ref.with_cap_exn sr @@ fun cap ->
  Logger.log cap msg

let () =
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  let net = env#net in
  start_server ~sw net;
  run_client ~sw ~net "./alice.cap" "Message from Alice";
  run_client ~sw ~net "./bob.cap" "Message from Bob";
  raise Exit
```

<!-- $MDX dir=examples/sturdy-refs -->
```sh
$ dune exec ./main.exe
[server] saved "alice.cap"
[server] saved "bob.cap"
[client] loaded "./alice.cap"
[server] "alice" says "Message from Alice"
[client] loaded "./bob.cap"
[server] "bob" says "Message from Bob"
Fatal error: exception Stdlib.Exit
[2]
```

### Creating services dynamically

So far, we have been providing a static set of sturdy refs.
We can also generate new services, with new sturdy refs, dynamically, and return them to clients.
Let's allow holders of a logger to create nested sub-loggers.
Then the admin can create `alice.cap` and `bob.cap` using the API, instead of hard-coding them,
and Alice and Bob can create further delegate to other users as needed.

<!-- $MDX include,file=examples/sturdy-refs-2/api.capnp -->
```capnp
@0x9ab198a53301be6e;

interface Logger {
  log @0 (msg :Text) -> ();
  sub @1 (label :Text) -> (logger :Logger);
}
```

To begin with, we'll just create live refs dynamically.
We can use the new API like this:

<!-- $MDX include,file=examples/sturdy-refs-2/main.ml,part=main -->
```ocaml
let () =
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  let net = env#net in
  let root_uri = start_server ~sw net in
  let vat = Capnp_rpc_unix.client_only_vat ~sw net in
  let root_sr = Capnp_rpc_unix.Vat.import vat root_uri |> or_fail in
  Sturdy_ref.with_cap_exn root_sr @@ fun root ->
  Logger.log root "Message from Admin";
  let for_alice = Logger.sub root "alice" in
  let for_bob = Logger.sub root "bob" in
  Logger.log for_alice "Message from Alice";
  Logger.log for_bob "Message from Bob";
  raise Exit
```

<!-- $MDX dir=examples/sturdy-refs-2 -->
```sh
$ dune exec ./main.exe
[server] "root" says "Message from Admin"
[server] "root/alice" says "Message from Alice"
[server] "root/bob" says "Message from Bob"
Fatal error: exception Stdlib.Exit
[2]
```

### The Persistence API

However, we'd like to be able to turn these live capabilities (`for_alice` and `for_bob`)
into URLs that we can send to Alice and Bob over some other transport (e.g. ssh or email).

Cap'n Proto defines a standard [Persistence API][] which services can implement
to allow clients to request their sturdy ref.

On the client side, calling `Persistence.save_exn cap` will send a request to `cap`
asking for its sturdy ref. For example, after getting a live capability,
the admin can request the sturdy ref like this:

<!-- $MDX include,file=examples/sturdy-refs-3/main.ml,part=save -->
```ocaml
  (* The admin creates a logger for Alice and saves it: *)
  let uri = Capability.with_ref (Logger.sub root "alice") Persistence.save_exn in
  Capnp_rpc_unix.Cap_file.save_uri uri "alice.cap" |> or_fail;
  (* Alice uses it: *)
  run_client ~sw ~net "alice.cap";
  raise Exit
```

If successful, the client can use this sturdy ref to connect directly to the logger in future:

<!-- $MDX dir=examples/sturdy-refs-3 -->
```sh
$ dune exec ./main.exe
[server] "root" says "Message from Admin"
[server] "root/alice" says "Message from Alice"
Fatal error: exception Stdlib.Exit
[2]
```

If you try the above, it will fail with `Unimplemented: Unknown interface 14468694717054801553UL`.
To add support on the server side, we must tell each logger instance what its public address is
and have it implement the persistence interface.
The simplest way to do this is to wrap the `Callback.local` call with `Persistence.with_sturdy_ref`:

<!-- $MDX include,file=examples/sturdy-refs-3/logger.ml,part=local -->
```ocaml
let rec local ~services sr label =
  let module Logger = Api.Service.Logger in
  Persistence.with_sturdy_ref sr Logger.local @@ object
```

Then pass the `services` and `sr` arguments when creating the logger:

<!-- $MDX include,file=examples/sturdy-refs-3/main.ml,part=root -->
```ocaml
  let root_id = Capnp_rpc_unix.Vat_config.derived_id config "root" in
  let root =
    let sr = Capnp_rpc_net.Restorer.Table.sturdy_ref services root_id in
    Logger.local ~services sr "root"
  in
```

### Persisting sturdy refs

Currently, each time we run the server we generate a new ID for Alice's logger
(`alice.cap` will be different each time).
For a real service, we will want to persist the IDs somehow.

`Table.add` is not a good choice for dynamic services because
it requires all capabilities to be loaded into the table at start-up,
which may be a performance problem.

Instead, we can create the table using `Table.of_loader`.
When the user asks for a sturdy ref that is not in the table,
it calls our `load` function to load the capability dynamically
from storage
(you can still use `Table.add` to register static services, as before).

A database such as sqlite3 is often a good choice for the dynamic services,
but as Cap'n Proto is also a useful on-disk format, we'll just use that in this guide.

Here's the interface for a `Db` module that loads and saves loggers:

<!-- $MDX include,file=examples/sturdy-refs-4/db.mli -->
```ocaml
open Capnp_rpc_lwt
open Capnp_rpc_net

include Restorer.LOADER

type loader = [`Logger_beacebd78653e9af] Sturdy_ref.t -> label:string -> Restorer.resolution
(** A function to create a new in-memory logger with the given label and sturdy-ref. *)

val create : make_sturdy:(Restorer.Id.t -> Uri.t) -> _ Eio.Path.t -> t * loader Eio.Promise.u
(** [create ~make_sturdy dir] is a database that persists services in [dir] and
    a resolver to let you set the loader (we're not ready to set the loader
    when we create the database). *)

val save_new : t -> label:string -> Restorer.Id.t
(** [save_new t ~label] adds a new logger with label [label] to the store and
    returns its newly-generated ID. *)
```

There is a `Capnp_rpc_unix.File_store` module that can persist Cap'n Proto structs to disk.
First, define a suitable Cap'n Proto data structure to hold the information we need to store.
In this case, it's just the label:

<!-- $MDX include,file=examples/sturdy-refs-4/store.capnp -->
```capnp
@0x97ed384f584feb4a;

struct SavedLogger {
  label @0 :Text;
}

struct SavedService {
  logger @0 :SavedLogger;
}
```

Using Cap'n Proto for this makes it easy to add extra fields or service types later if needed
(`SavedService.logger` can be upgraded to a union if we decide to add more service types later).
We can use this with `File_store` to implement `Db`:

<!-- $MDX include,file=examples/sturdy-refs-4/db.ml -->
```ocaml
open Eio.Std
open Capnp_rpc_lwt
open Capnp_rpc_net

module File_store = Capnp_rpc_unix.File_store
module Store = Store.Make(Capnp.BytesMessage)

type loader = [`Logger_beacebd78653e9af] Sturdy_ref.t -> label:string -> Restorer.resolution

type t = {
  store : Store.Reader.SavedService.struct_t File_store.t;
  loader : loader Promise.t;
  make_sturdy : Restorer.Id.t -> Uri.t;
}

let hash _ = `SHA256

let make_sturdy t = t.make_sturdy

let save t ~digest label =
  let open Store.Builder in
  let service = SavedService.init_root () in
  let logger = SavedService.logger_init service in
  SavedLogger.label_set logger label;
  File_store.save t.store ~digest @@ SavedService.to_reader service

let save_new t ~label =
  let id = Restorer.Id.generate () in
  let digest = Restorer.Id.digest (hash t) id in
  save t ~digest label;
  id

let load t sr digest =
  match File_store.load t.store ~digest with
  | None -> Restorer.unknown_service_id
  | Some saved_service ->
    let logger = Store.Reader.SavedService.logger_get saved_service in
    let label = Store.Reader.SavedLogger.label_get logger in
    let sr = Capnp_rpc_lwt.Sturdy_ref.cast sr in
    let loader = Promise.await t.loader in
    loader sr ~label

let create ~make_sturdy dir =
  let loader, set_loader = Promise.create () in
  if not (Eio.Path.is_directory dir) then
    Eio.Path.mkdir dir ~perm:0o755;
  let store = File_store.create dir in
  {store; loader; make_sturdy}, set_loader
```

Note: to avoid possible timing attacks, the `load` function is called with the digest of the service ID rather than with the ID itself. This means that even if the load function takes a different amount of time to respond depending on how much of a valid ID the client guessed, the client will only learn the digest (which is of no use to them), not the ID.
The file store uses the digest as the filename, which avoids needing to check the ID the client gives for special characters, and also means that someone getting a copy of the store (e.g. an old backup) doesn't get the IDs (which would allow them to access the real service).

The main `start_server` function then uses `Db` to create the table:

<!-- $MDX include,file=examples/sturdy-refs-4/main.ml,part=server -->
```ocaml
let serve config =
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  (* Create the on-disk store *)
  let make_sturdy = Capnp_rpc_unix.Vat_config.sturdy_uri config in
  let db, set_loader = Db.create ~make_sturdy (env#cwd / "store") in
  (* Create the restorer *)
  let services = Restorer.Table.of_loader ~sw (module Db) db in
  Switch.on_release sw (fun () -> Restorer.Table.clear services);
  let restore = Restorer.of_table services in
  (* Add the root service *)
  let persist_new ~label =
    let id = Db.save_new db ~label in
    Capnp_rpc_net.Restorer.restore restore id
  in
  let root_id = Capnp_rpc_unix.Vat_config.derived_id config "root" in
  let root =
    let sr = Capnp_rpc_net.Restorer.Table.sturdy_ref services root_id in
    Logger.local ~persist_new sr "root"
  in
  Restorer.Table.add services root_id root;
  (* Tell the database how to restore saved loggers *)
  Promise.resolve set_loader (fun sr ~label -> Restorer.grant @@ Logger.local ~persist_new sr label);
  (* Run the server *)
  let _vat = Capnp_rpc_unix.serve ~sw ~net:env#net ~restore config in
  let uri = Capnp_rpc_unix.Vat_config.sturdy_uri config root_id in
  Capnp_rpc_unix.Cap_file.save_uri uri "admin.cap" |> or_fail;
  print_endline "Wrote admin.cap";
  Fiber.await_cancel ()
```

The server implementation of the `sub` method gets the label from the parameters
and uses `persist_new` to save the new logger to the database:

<!-- $MDX include,file=examples/sturdy-refs-4/logger.ml,part=sub-impl -->
```ocaml
    method sub_impl params release_param_caps =
      let open Logger.Sub in
      let sub_label = Params.label_get params in
      release_param_caps ();
      let label = Printf.sprintf "%s/%s" label sub_label in
      match persist_new ~label with
      | Error e -> Service.error (`Exception e)
      | Ok logger ->
        let response, results = Service.Response.create Results.init_pointer in
        Results.logger_set results (Some logger);
        Capability.dec_ref logger;
        Service.return response
```

<!-- $MDX dir=examples/sturdy-refs-4,non-deterministic -->
```sh
$ dune exec -- ./main.exe serve --capnp-secret-key=server.key --capnp-listen-address unix:/tmp/demo.sock &
Wrote admin.cap

$ dune exec -- ./main.exe log admin.cap "Hello from Admin"
[server] "root" says "Hello from Admin"

$ dune exec -- ./main.exe sub admin.cap alice
Wrote "alice.cap"

$ dune exec -- ./main.exe log alice.cap "Hello from Alice"
[server] "root/alice" says "Hello from Alice"

$ dune exec -- ./main.exe sub alice.cap bob
Wrote "bob.cap"

$ dune exec ./main.exe log bob.cap "Hello from Bob"
[server] "root/alice/bob" says "Hello from Bob"
```

You should find that the loggers now persist even if the service is restarted.


## Further reading

* [`capnp_rpc_lwt.mli`](capnp-rpc-lwt/capnp_rpc_lwt.mli) and [`s.ml`](capnp-rpc-lwt/s.ml) describe the OCaml API.
* [Cap'n Proto schema file format][schema] shows how to build more complex structures, and the "Evolving Your Protocol" section explains how to change the schema without breaking backwards compatibility.
* <https://discuss.ocaml.org/> is a good place to ask questions (tag them as "capnp").
* [The capnp-ocaml site][capnp-ocaml] explains how to read and build more complex types using the OCaml interface.
* [E Reference Mechanics][] gives some insight into how distributed promises work.

## FAQ

### Why does my connection stop working after 10 minutes?

Cap'n Proto connections are often idle for long periods of time, and some networks automatically close idle connections.
To avoid this, capnp-rpc-unix sets the `SO_KEEPALIVE` option when connecting to another vat,
so that the initiator of the connection will send a TCP keep-alive message at regular intervals.
However, TCP keep-alives are sent after the connection has been idle for 2 hours by default,
and this isn't frequent enough for e.g. Docker's libnetwork,
which silently breaks idle TCP connections after about 10 minutes.

A typical sequence looks like this:

1. A client connects to a server and configures a notification callback.
2. The connection is idle for 10 minutes. libnetwork removes the connection from its routing table.
3. Later, the server tries to send the notification and discovers that the connection has failed.
4. After 2 hours, the client sends a keep-alive message and it too discovers that the connection has failed.
   It establishes a new connection and retries.

On some platforms, capnp-rpc-unix (>= 0.9.0) is able to reduce the timeout to 1 minute by setting the `TCP_KEEPIDLE` socket option.
On other platforms, you may have to configure this setting globally (e.g. with `sudo sysctl net.ipv4.tcp_keepalive_time=60`).

### How can I return multiple results?

Every Cap'n Proto method returns a struct, although the examples in this README only use a single field.
You can return multiple fields by defining a method as e.g. `-> (foo :Foo, bar :Bar)`.
For more complex types, it may be more convenient to define the structure elsewhere and then refer to it as
`-> MyResults`.

### Can I create multiple instances of an interface dynamically?

Yes. e.g. in the example above we can use `Callback.local fn` many times to create multiple loggers.
Just remember to call `Capability.dec_ref` on them when you're finished so that they can be released
promptly (but if the TCP connection is closed, all references on it will be freed anyway).
Using `Capability.with_ref` makes it easier to ensure that `dec_ref` gets called in all cases.

### Can I get debug output?

First, always make sure logging is enabled so you can at least see warnings.
The `main.ml` examples in this document enable some basic logging.

If you turn up the log level to `Debug`, you'll see lots of information about what is going on.
Turning on colour in the logs will help too - see `test-bin/calc.ml` for an example.

Many references will be displayed with their reference count (e.g. as `rc=3`).
You can also print a capability for debugging with `Capability.pp`.

`CapTP.dump` will dump out the state of an entire connection,
which will show you what services you’re currently importing and exporting over the connection.

If you override your service’s `pp` method, you can include extra information in the output too.
Use `Capnp_rpc.Debug.OID` to generate and display a unique object identifier for logging.

### How can I debug reference counting problems?

If a capability gets GC'd with a non-zero ref-count, you should get a warning.
For testing, you can use `Gc.full_major` to force a check.

If you try to use something after releasing it, you'll get an error.

But the simple rule is: any time you create a local capability or extract a capability from a message,
you must eventually call `Capability.dec_ref` on it.

### How can I import a sturdy ref that I need to start my vat?

Let's say you have a capnp service that internally requires the use of another capnp service:

<p align='center'>
  <img src="./diagrams/three_vats.svg"/>
</p>

Here, creating the `Frontend` service requires a sturdy ref for the `Backend` service.
But this sturdy ref must be imported into the frontend vat.
Creating the frontend vat requires passing a restorer, which needs `Frontend`!

The solution here is to construct `Frontend` with a *promise* for the sturdy ref, e.g.

<!-- $MDX skip -->
```ocaml
let run_frontend ~sw ~net backend_uri =
  let backend_promise, resolver = Promise.create () in
  let frontend = Frontend.make backend_promise in
  let restore = Restorer.single id frontend in
  let vat = Capnp_rpc_unix.serve ~sw ~net ~restore config in
  Promise.resolve resolver (Capnp_rpc_unix.Vat.import_exn vat backend_uri)
```

### How can I release other resources when my service is released?

Override the `release` method. It gets called when there are no more references to your service.

### Is there an interactive version I can use for debugging?

[The Python bindings][pycapnp] provide a good interactive environment.
For example, start the test service above and leave it running:

<!-- $MDX skip -->
```
$ ./_build/default/main.exe
Connecting to server at capnp://insecure@127.0.0.1:7000
[...]
```

Note that you must run without encryption for this, and use a non-secret ID:

<!-- $MDX skip -->
```ocaml
let config = Capnp_rpc_unix.Vat_config.create ~serve_tls:false ~secret_key listen_address in
let service_id = Restorer.Id.public "" in
```

Run `python` from the directory containing your `echo_api.capnp` file and do:

```python
import capnp
import echo_api_capnp
client = capnp.TwoPartyClient('127.0.0.1:7000')
echo = client.bootstrap().cast_as(echo_api_capnp.Echo)
```

Importing a module named `foo_capnp` will load the Cap'n Proto schema file `foo.capnp`.

To call the `ping` method:

```python
echo.ping("From Python").wait()
```
    <echo_api_capnp:Echo.ping$Results reader (reply = "echo:From Python")>

To call the heartbeat method, with results going to the server's own logger:

```python
echo.heartbeat("From Python", echo.getLogger().callback).wait()
```
    Service logger: "From Python"

To call the heartbeat method, with results going to a Python callback:

```python
class CallbackImpl(echo_api_capnp.Callback.Server):
    def log(self, msg, _context): print("Python callback got %s" % msg)

echo.heartbeat("From Python", CallbackImpl())
capnp.wait_forever()
```
    Python callback got From Python
    Python callback got From Python
    Python callback got From Python

Note that calling `wait_forever` prevents further use of the session, however.

### Can I set up a direct 2-party connection over a pre-existing channel?

The normal way to connect to a remote service is using a sturdy ref, as described above.
This uses the [NETWORK][] to open a new connection to the server, or reuses an existing connection
if there is one. However, it is sometimes useful to use a pre-existing connection directly.

For example, a process may want to spawn a child process and communicate with it
over a socketpair. The [calc_direct.ml][] example shows how to do this:

```
$ dune exec -- ./test-bin/calc_direct.exe
parent: application: Connecting to child process...
parent: application: Sending request...
 child: application: Serving requests...
 child: application: 21.000000 op 2.000000 -> 42.000000
parent: application: Result: 42.000000
parent: application: Shutting down...
parent:   capnp-rpc: Connection closed
parent: application: Waiting for child to exit...
parent: application: Done
```

## Contributing

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

```yaml
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

A call also takes a *resolver*, which it will call with the answer when it's ready.
The answer will also contain data and pointer parts.

On top of this basic model the Cap'n Proto schema compiler ([capnp-ocaml]) generates a typed API, so that application code can only generate or attempt to consume messages that match the schema.
Application code does not need to worry about interface or method IDs, for example.

This might seem like a rather clumsy system, but it has the advantage that such messages can be sent not just within a process,
like regular OCaml method calls, but also over the network to remote objects.

The network is made up of communicating "vats" of objects.
You can think of a Unix process as a single vat.
The vats are peers - there is no difference between a "client" and a "server" at the protocol level.
However, some vats may not be listening for incoming network connections, and you might like to think of such vats  as clients.

When a connection is established between two vats, each can choose to ask the other for access to some service.
Services are usually identified by a long random secret (a "Swiss number") so that only authorised clients can get access to them.
The capability they get back is a proxy object that acts like a local service but forwards all calls over the network.
When a message is sent that contains pointers, the RPC system holds onto the pointers and makes each object available over that network connection.
Each vat only needs to expose at most a single bootstrap object,
since the bootstrap object can provide methods to get access to any other required services.

All shared objects are scoped to the network connection, and will be released if the connection is closed for any reason.

The RPC system is smart enough that if you export a local object to a remote service and it later exports the same object back to you, it will switch to sending directly to the local service (once any pipelined messages in flight have been delivered).

You can also export an object that you received from a third-party, and the receiver will be able to use it.
Ideally, the receiver should be able to establish a direct connection to the third-party, but
this isn't yet implemented and instead the RPC system will forward messages and responses in this case.


### Building

To build:

    git clone https://github.com/mirage/capnp-rpc.git
    cd capnp-rpc
    opam pin add -ny .
    opam depext -t capnp-rpc-unix capnp-rpc-mirage
    opam install --deps-only -t .
    make test

If you have trouble building, you can use the Dockerfile shown in the CI logs (click the green tick on the main page).

### Testing

Running `make test` will run through the tests in `test-lwt/test.ml`, which run some in-process examples.

The calculator example can also be run across two Unix processes.

Start the server with:

```
$ dune exec -- ./test-bin/calc.exe serve \
    --capnp-listen-address unix:/tmp/calc.socket \
    --capnp-secret-key-file=key.pem
Waiting for incoming connections at:
capnp://sha-256:LPp-7l74zqvGcRgcP8b7-kdSpwwzxlA555lYC8W8prc@/tmp/calc.socket
```

Note that `key.pem` does not need to exist. A new key will be generated and saved if the file does not yet exist.

In another terminal, run the client and connect to the address displayed by the server:

```
dune exec -- ./test-bin/calc.exe connect capnp://sha-256:LPp-7l74zqvGcRgcP8b7-kdSpwwzxlA555lYC8W8prc@/tmp/calc.socket/
```

You can also use `--capnp-disable-tls` if you prefer to run without encryption
(e.g. for interoperability with another Cap'n Proto implementation that doesn't support TLS).
In that case, the client URL would be `capnp://insecure@/tmp/calc.socket`.

### Fuzzing

Running `make fuzz` will run the AFL fuzz tester. You will need to use a version of the OCaml compiler with AFL support (e.g. `opam sw 4.04.0+afl`).

The fuzzing code is in the `fuzz` directory.
The tests set up some vats in a single process and then have them perform operations based on input from the fuzzer.
At each step it selects one vat and performs a random (fuzzer-chosen) operation out of:

1. Request a bootstrap capability from a random peer.
2. Handle one message on an incoming queue.
3. Call a random capability, passing randomly-selected capabilities as arguments.
4. Finish a random question.
5. Release a random capability.
6. Add a capability to a new local service.
7. Answer a random question, passing random-selected capability as the response.

The content of each call is a (mutable) record with counters for messages sent and received on the capability reference used.
This is used to check that messages arrive in the expected order.

The tests also set up a shadow reference graph, which is like the regular object capability reference graph except that references between vats are just regular OCaml pointers (this is only possible because all the tests run in a single process, of course).
When a message arrives, the tests compare the service that the CapTP network handler selected as the target with the expected target in this simpler shadow network.
This should ensure that messages always arrive at the correct target.

In future, more properties should be tested (e.g. forked references, that messages always eventually arrive when there are no cycles, etc).
We should also test with some malicious vats (that don't follow the protocol correctly).

[schema]: https://capnproto.org/language.html
[capnp-ocaml]: https://github.com/pelzlpj/capnp-ocaml
[Cap'n Proto]: https://capnproto.org/
[Cap'n Proto RPC Protocol]: https://capnproto.org/rpc.html
[E-Order]: http://erights.org/elib/concurrency/partial-order.html
[E Reference Mechanics]: http://www.erights.org/elib/concurrency/refmech.html
[pycapnp]: http://jparyani.github.io/pycapnp/
[Persistence API]: https://github.com/capnproto/capnproto/blob/master/c%2B%2B/src/capnp/persistent.capnp
[Mirage]: https://mirage.io/
[ocaml-ci]: https://github.com/ocaml-ci/ocaml-ci
[api]: https://mirage.github.io/capnp-rpc/
[NETWORK]: https://mirage.github.io/capnp-rpc/capnp-rpc-net/Capnp_rpc_net/S/module-type-NETWORK/index.html
[calc_direct.ml]: ./test-bin/calc_direct.ml
