@0x9ab198a53301be6e;

interface Logger {
  log @0 (msg :Text) -> ();
  sub @1 (label :Text) -> (logger :Logger);
}
