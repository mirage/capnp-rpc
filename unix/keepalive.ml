let have_tcp_keepidle =
  try ExtUnix.All.have_sockopt_int ExtUnix.All.TCP_KEEPIDLE
  with ExtUnix.All.Not_available _ -> false

let try_set_idle socket i =
  if have_tcp_keepidle then (
    ExtUnix.All.setsockopt_int socket ExtUnix.All.TCP_KEEPIDLE i
  )
