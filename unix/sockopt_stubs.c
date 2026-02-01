#include <caml/mlvalues.h>
#include <caml/memory.h>

#ifdef _WIN32
#include <winsock2.h>
#include <ws2tcpip.h>
#define KEEPIDLE_OPT (-1)
#else
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>

/* macOS uses TCP_KEEPALIVE instead of TCP_KEEPIDLE */
#if defined(TCP_KEEPIDLE)
#define KEEPIDLE_OPT TCP_KEEPIDLE
#elif defined(TCP_KEEPALIVE)
#define KEEPIDLE_OPT TCP_KEEPALIVE
#else
#define KEEPIDLE_OPT (-1)
#endif
#endif

CAMLprim value capnp_rpc_set_keepidle(value v_fd, value v_time)
{
  CAMLparam2(v_fd, v_time);
#if KEEPIDLE_OPT == (-1)
  (void)v_fd;
  (void)v_time;
#else
#ifdef _WIN32
  SOCKET fd = Handle_val(v_fd);
#else
  int fd = Int_val(v_fd);
#endif
  int time = Int_val(v_time);
  setsockopt(fd, IPPROTO_TCP, KEEPIDLE_OPT, &time, sizeof(time));
#endif
  CAMLreturn(Val_unit);
}
