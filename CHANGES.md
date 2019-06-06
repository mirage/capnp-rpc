### v0.3.2

- Update for various upstream API changes, switch to the opam 2 metadata
  format, and convert from jbuilder to dune (@talex5, #152).

- Adjust to mirage-stack / mirage-protocols changes (Nick Betteridge, #151).
  * update mirage/network for upgraded Ipaddr
  * update Dockerfile to use opam2, apt-get update, and newer opam-repository

- Update dependencies from opam-repository (@talex5, #148).

### 0.3.1

- Updates for new `x509` API and for OCaml 4.06 (#143).

- Add some diagrams to the tutorial (#134).

- Add FAQ: how can I import a sturdy ref that I need to start my vat? (#137)

Build updates:

- Add build dependency on conf-capnproto (#146). Projects using the schema compiler themselves should also now add this dependency instead of relying on `capnp` to pull it in.

- Remove generics from persistent.capnp (#141) so that it compiles on systems with older capnproto compilers (e.g. Ubuntu 14.04).

- `unix/network.ml` uses `Fmt.failwith`, which requires fmt.0.8.4 (#139).

- `capnp-rpc-lwt` requires `Uri.with_userinfo`, which is only in `uri` >= 1.6.0 (#138).

- Move test-lwt to unix module (#133).


### 0.3 Unikernels

This release adds a new `capnp-rpc-mirage` package, which provides support for
using the library within a MirageOS unikernel.
See <https://github.com/mirage/capnp-rpc#how-can-i-use-this-with-mirage> for details.

There are a few minor API changes:

- `Capnp_rpc_unix.Vat_config.derived_id ?name config` is now
  `Capnp_rpc_unix.Vat_config.derived_id config name`.
  If you weren't passing a `~name` argument before, use `"main"` to get the same ID.

- `Capnp_rpc_unix.Network`'s `Socket_address` module is now called `Location`.

- There is an explicit network parameter in `Network.connect`, etc.
  This is needed to support Mirage, where the network isn't a global.

Bug fixes:

- Fix race when reconnecting.
  We notified the user that the capability had broken while
  the old connection was still shutting down.
  If they immediately tried to reconnect, we tried to reuse the old connection.
  Now, we wait for it to be removed.

- Fix handling of leaks in switchable.
  If we detected the ref-count was invalid, we tried to resolve to an error,
  but resolving now checks that the ref-count is valid first so this failed.

Documentation and examples:

- Fixed ref-counting bug in calculator example.
  Also, changed the service ID to match what the C++ client expects.
  With these changes, the C++ client's tests pass when used with the OCaml service.

Fuzzing:

- Also test answering questions with errors or with a promise from another question.

Code cleanups:

- Use a better way to get the client certificate from a TLS connection
  (suggested by @hannesm).

- Use `Alcotest_lwt` for unit-tests.

- Move `capnp://` URI handling to `Capnp_rpc_lwt.Capnp_address`.
  This allows it to be shared with the Mirage code.

- Add `Capnp_rpc_lwt.VAT_NETWORK` with simpler signature than `S.VAT_NETWORK`.

- The address sub-module of `S.NETWORK` is now available separately as `S.ADDRESS`.


### 0.2 Persistence, encryption and access control

This release brings support for RPC Level 2.

The API for implementing services and clients is mostly unchanged,
but the APIs for setting up networking are very different.
If you read the tutorial with the 0.1 release, you will probably
want to read the new version again from the "Networking" point
onwards.

The main change is that when connecting to a service you now give a URI of the form:

`capnp://hash:digest@address/service`

The client will connect to `address`, check the server's public key matches `hash:digest`,
and then pass the (secret) `service` ID to get access to a particular service.
The server will typically display the URI to use on start-up, or write it to a file.

The communications are encrypted using TLS.
If you want to disable TLS, use the form `capnp://insecure@address`.
This should only be needed for interoperability with non-TLS services,
as the system will generate keys and certificates automatically,
making secure use just as easy as the non-secure case.

The other major new feature is support for persistent services.
In version 0.1 you could specify an `offer` argument when creating a vat,
telling it a service to provide in response to bootstrap requests.
Now, you pass a `restore` argument, which can restore different services
depending on the service ID provided by the client.

The new `Restorer.Table` module provides a table-based lookup restorer,
to which services can be added dynamically.
If you have a lot of services and don't want to add them all at startup,
you can use `Restorer.Table.of_loader` and provide your own function for loading services.

#### Other changes

Documentation changes:

- The tutorial has been extended and a FAQ added.

- The recommended layout of protocol files has changed.
  The `Client` sub-module is gone, and `service` becomes `local`.

- The examples now have `.mli` files and there is a new `store.ml` example demonstrating persistence.
  The examples have been updated to the new layout convention.

Main API changes:

- The `Capnp_rpc_lwt.Capability` module adds some useful functions:
  - `broken` creates a broken capability.
  - `when_broken` allows you to be notified when a capability breaks (e.g. because of a network failure).
  - `wait_until_settled` waits until a promise has resolved, if you don't want to pipeline (e.g. you want to send a large amount of data, so prefer to find out where the service is and avoid any forwarding).
  - `equal` tests if two capabilities designate the same service.

- The new `Capnp_rpc_lwt.Sturdy_ref` module provides an abstraction for off-line capabilities.
  `Sturdy_ref.connect` can be used to get a live connection.
  If you try to connect to multiple services in the same vat, it will share a single connection automatically.
  `Sturdy_ref.reader` and `Sturdy_ref.builder` can be used for passing sturdy refs in messages.

- The new `Capnp_rpc_lwt.Restorer` module is used to implement sturdy-refs at the hosting side.

- The new `Capnp_rpc_lwt.Persistence` module provides support for the Cap'n Proto persistence protocol.
  Clients use `Persistence.save` to request a sturdy ref from a service, and
  services can use `Persistence.with_sturdy_ref` to answer such requests automatically.

- The new `Capnp_rpc_unix.Vat_config` collects together all vat configuration in one place.

- The new `Capnp_rpc_unix.File_store` can store Cap'n Proto structs in a directory.
  It can be useful when implementing persistence.

- The new `Capnp_rpc_lwt.Auth` module provides support for generating and handling secret keys and fingerprints.

- The new `Capnp_rpc_lwt.Tls_wrapper` provides support for doing TLS handshakes, with authentication and encryption.

In the core `capnp-rpc` package (which applications do not normally use directly):

- The new `cap#when_released` method can be used to find out when a capability is no longer needed.
  The restorer uses this to remove dynamically loaded services from its table automatically.

- The new `when_broken` helper adds a callback to call when a promise or far-ref becomes broken.

- `NETWORK_TYPES` is now separate from `CORE_TYPES`.

### 0.1

- Initial release. RPC Level 1 is fully implemented.
