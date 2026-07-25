let try_set socket =
  try
    Eio.Net.setsockopt socket Eio.Net.Sockopt.SO_KEEPALIVE true;
    Eio.Net.setsockopt socket Eio.Net.Sockopt.TCP_KEEPIDLE 60;
  with Eio.Io _ -> ()
