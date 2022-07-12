@0x9d72e60df69f49b0;

interface Echo {
  ping @0 (msg :Text, slow :Bool) -> (reply :Text);
  unblock @1 ();
}

interface Registry {
  setEchoService @2 (service :Echo) -> ();
  echoService @0 () -> (service :Echo);
  echoServicePromise @4 () -> (service :Echo);
  unblock @1 ();
  complex @3 () -> (foo :Foo, bar :Bar);
}

interface Version {
  read @0 () -> (version :Text);
}

struct Foo {
  b @0 :Bar;
  echo @1 :Echo;
}

struct Bar {
  f @0 :Foo;
  version @1 :Version;
}

interface File {
  set @0 (data :Text) -> ();
  get @1 () -> (data :Text);
}

interface Store {
  createFile @0 () -> (file :File);
}

struct Simple {
  text @0 :Text;
}
