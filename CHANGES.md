### v2.x

- Rename `Capnp_rpc_lwt` to `Capnp_rpc` (which is now `Capnp_rpc_proto`).
  The new `Capnp_rpc` now provides `Error`, `Exception` and `Debug` aliases
  to the same modules in `Capnp_rpc_proto`, so that `Capnp_rpc_proto` doesn't
  need to be used directly.

- Add `Capnp_rpc.Std` with some common module aliases, to reduce the need
  to `open Capnp_rpc` (which is rather large).

- Convert API from Lwt to Eio.

To update to the new API:

1. Use [lwt_eio][] during the migration to allow using Eio and Lwt together in your application.
2. Replace `open Capnp_rpc_lwt` with `open Capnp_rpc.Std`.
3. Replace all other uses of `Capnp_rpc_lwt` with just `Capnp_rpc`.
4. In `dune` and `opam` files, replace `capnp-rpc-lwt` with `capnp-rpc`.
5. Some modules are in `Capnp_rpc` but not the `Capnp_rpc.Std` subset.
   Those should now be fully qualified (e.g. replace `Persistence` with
   `Capnp_rpc.Persistence`).
6. Replace `Service.return_lwt` with `Lwt_eio.run_lwt`.
7. Once all Lwt code is gone, `lwt_eio` can be removed.

[lwt_eio]: https://github.com/ocaml-multicore/lwt_eio

### v1.2.3

- Update to cmdliner 1.1.0 (@MisterDA #249).

- Fix tests on Windows, avoid using Unix domain sockets (@talex5 #251).

- Fix warning on `Unix.ENOTCONN` under macos (@talex5 #252).

- Add `Capnp_rpc_unix.Cap_file.save_uri` (@talex5 #253).

- Update README to use MDX (@talex5 #254).

- Update tests to Cap'n Proto 0.10.3 (@MisterDA #257 #260).

- Update to mirage-crypto-rng 0.11 and dns 7.0.0 API changes (@hannesm #261).

- Update for opam 2.1+ installations (@frumioj #259).

- Install Cap'n Proto binaries in Windows GHA (@MisterDA #248).

### v1.2.2

- Switch from mirage-{stack,protocols} to tcpip (@MisterDA #246).

- Remove deprecated tcpip.unix dependency (@hannesm #245).

- Add OCaml 4.14 support (@kit-ty-kate #244).

### v1.2.1

- Fix Fmt deprecation warnings (@tmcgilchrist #239).

- Update to latest X509 and DNS APIs (@tmcgilchrist #240).

### v1.2

- Don't crash if the peer disconnects before the bootstrap reply is ready (@talex5 #234).
  When replying to a normal question we checked that the answer was still needed before trying to reply, but didn't for bootstrap requests.

- Replace `wait_until_settled` with safer `await_settled` (@talex5 #233).
  `wait_until_settled` makes it too easy to ignore errors.

- Require fmt >= 0.8.7 for `kstr` and set OCaml 4.08 as the minimum version everywhere (@talex5 #232).
  Older versions aren't tested in CI because the unix and mirage packages require at least OCaml 4.08.

### v1.1

- Update to latest X509, TCP and TLS APIs (@talex5 @hannesm #228).

- Add `Service.fail_lwt` convenience function (@talex5 #229).

- Remove confusing debug details from `call_for_value_exn` errors (@talex5 #230).
  The hidden information is now logged (at debug level) instead.

- Configure TCP keep-alives for incoming connections, not just outgoing ones (@talex5 #227).
  This is needed when the client machine crashes without resetting the connection.

- Include version number in opam license field (@talex5 #226).

### v1.0

- Skip the setting of `object_id` if it is empty (@LasseBlaauwbroek #224).
  This improves interoperability with the C++ implementation.

- Use `open_in_bin` instead of `open_in` for Windows compatibility (@MisterDA #222).

### v0.9.0

- Add connection progress indicator (@talex5 #220).  
  Clients can now use `Capnp_rpc_unix.with_cap_exn` to show a progress indicator while waiting to connect.
  It will use the log if enabled.
  If not, but stderr is a tty, it shows a message while connecting and then erases it (if connecting takes longer than 0.5s).
  If stderr is not a tty, it just prints a message there.

- Use `Mirage_crypto_rng_lwt.initialize` instead of `Mirage_crypto_rng_unix.initialize` (@hannesm #217).  
  The former periodically feeds entropy to the RNG, while the latter seeds the RNG only once.

- Set TCP_KEEPIDLE=60 if possible (@talex5 #214).  
  Cap'n Proto connections tend to be long lived and we therefore turn on the `SO_KEEPALIVE` option.
  However, the default keepalive timeout of 2 hours is much too long for some networks.
  In particular, Docker's libnetwork silently drops idle connections after about 10 minutes.

Windows support:

- Prevent crash if ExtUnix wasn't built with sockopt (@MisterDA #218).  
  If ExtUnix isn't built with sockopt then the symbol `have_sockopt_int` isn't available and an exception is raised.  
  This may be the case on some environments (e.g., ExtUnix built with mingw-w64).
  It is a workaround until support for `TCP_KEEPIDLE` is added for these environments (e.g., using Winsock instead).

- There is no SIGPIPE on Windows (@dra27 #212).

- Switch from "capnpc" to "capnp compile" (@talex5 #213).  
  `capnpc` seems to be the old name, and isn't present on Windows.

Build changes:

- Remove Travis and Dockerfile (@talex5 #220).  
  Travis is very slow and error-prone now, and ocaml-ci generates a working Dockerfile automatically for people who want it.

- Include transitive dependencies in opam files (@talex5 #219).  
  Patch generated by https://github.com/ocurrent/opam-dune-lint/

- Require `tcpip.unix` specifically to get the checksum stubs linked (@hannesm #216).  

### v0.8.0

Bug fixes and diagnostics:

- Don't GC imports with callbacks (@talex5, #210).

- Improve "Already resolved!" error message (@talex5, #209).
  Show the state of the promise we were trying to set, and what we tried to set it to.

- Reject attempts to send after disconnecting (@talex5, #208).

- Unix: don't leak FDs if `Network.connect` fails (@talex5, #206).
  Also, use `Lwt` to open connections so we don't block the whole process.

New functions:

- Add `Sturdy_ref.with_cap` and `with_cap_exn` convenience functions (@talex5, #207).
  Using the plain `connect` functions, it's easy to forget to release the live-ref at the end.

Build changes:

- capnp-rpc-mirage: adapt test to tcpip 5.0.0 API (@hannesm, #205).

- Upgrade to dune 2 (@talex5, #204).

- Switch tests to build in native code and use the test stanza (@avsm, #203).

Documentation:

- Show how to make a connection directly (@talex5, #202).
  The new `test-bin/calc_direct.ml` example shows how a parent process can
  spawn a child and communicate with it directly over a socketpair.

### v0.7.0

- Update for x509 0.11.0 API changes (@talex5, #196).

- Update to new mirage network API (@Cjen1, #198).

- Add echo benchmark test (@Cjen1, #197).

- Estimate message sizes to improve performance (@talex5, #200).
  By default, capnproto allocates 8k buffers, but most capnp-rpc messages are
  much smaller than this.

Logging:

- Fix application logging, to use library's log (@Cjen1, #195).

- Expose the endpoint logger (@Cjen1, #195).

- Only enable debug logging for capnp libraries in the calc example.
  TLS now generates a lot of messages at debug level (@talex5, #200).

### v0.6.0

- Port to latest interfaces for x509 (0.10+), mirage-crypto,
  alcotest (1.0+), and mirage-types post the `-lwt` package
  merge (@avsm #190, review by @talex5 @hannesm)

- Convert many info-level log messages to debug level (@talex5 #193)

### v0.5.0

Breaking changes:

- The networking parts of `capnp-rpc-lwt` have been split off into a new
  `capnp-rpc-net` package (@talex5, #182). This means that libraries that
  provide or consume capnp-rpc services can just depend on `capnp-rpc-lwt`,
  without pulling in a full TLS stack. Only applications should need to
  depend on `capnp-rpc-net` (which will probably happen automatically via
  `capnp-rpc-unix` or `capnp-rpc-mirage`). If you get compile errors in
  your code due to this change, you probably just need to
  `open Capnp_rpc_net`.

Other changes:

- Add Prometheus metrics for connections (@talex5, #183).  
  `capnp-rpc-net` now reports the current number of active connections,
  the total number of messages received, and
  the total number of messages enqueued, sent and dropped
  (from which you can work out the current number of queued messages).

- Adapt to x509 0.8.x API changes (@hannesm, #176).

- In the tutorial, say where to put the `Callback` module (@talex5, #177).

- "No client certificate found" should not be fatal (@talex5, #178).

- Remove unused dependency on `mirage-flow-unix` from opam file (@talex5,
  #179).

### v0.4.0

Breaking changes:

- Wrap errors with the `` `Capnp`` tag to make it easier to compose with other types of error (#172, #173).

- Prefix all command-line options with `capnp-` (#163).
  e.g. `--listen-address` is now `--capnp-listen-address`.
  The old names were confusing for applications that supported other protocols too (e.g. a web server).

New features:

- Add `Capability.with_ref` convenience function (#170).
  This automatically calls `dec_ref` when done.

- Add Unix `Cap_file` module to load and save `Sturdy_refs` (#165).
  In particular, this ensures that saved cap files get a mode of `0o600`, since they contain secrets.

- Export cmdliner network address parsing (#165).
  This is useful if you don't want to use the default option parsing.
  For example, if you want to make Cap'n Proto an optional feature of your program.

- Upgrade from `uint` (which is deprecated) to the newer `stdint` (#166, #168).
  The latest version of `uint` is just a wrapper around `stdint`,
  so this shouldn't break anything if you are using the latest version.

- Put cmdliner options in their own man-page section (#163).
  Use `Capnp_rpc_unix.manpage_capnp_options` to control where in your man-page
  they appear.

- Enable `SO_KEEPALIVE` for TCP connections (#167).
  For use with Docker's libnetwork, try something like this in your `stack.yml`:

  ```
  sysctls:
    - 'net.ipv4.tcp_keepalive_time=60'
  ```

Bug fixes:

- Close listening socket when shutting down a vat (#171).

- Don't mark secret keys as executable (#164).

- Update README example to use dune (#162).

Build changes:

- Replace topkg with dune-release (#169)

- Update opam email address and fix missing bound (#161).

- Update the `dune` files to allow duniverse / vendored builds (#165).

- Fix the crossed-calls unit test (#171).

- Force all capnp-rpc subpackages to have the same version (#173).

### v0.3.3

- Update uint.uint32 to uint (@Cjen1, #159).
- Update to new x509 API (@talex5, #158).
- Require base64 >= 3.0.0 for capnp-rpc-mirage too (@talex5, #157).
- Put test sockets in temporary directory (@talex5, #156).

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
