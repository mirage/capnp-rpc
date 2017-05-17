@0x9d72e60df69f49b0;

interface Echo {
  ping @0 (msg :Text, slow :Bool) -> (reply :Text);
  unblock @1 ();
}

interface Registry {
  echoService @0 () -> (service :Echo);
}
