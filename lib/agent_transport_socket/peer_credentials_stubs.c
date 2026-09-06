#define _GNU_SOURCE
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

static void ochat_peer_fail(const char *operation)
{
  char message[256];
  snprintf(message, sizeof(message), "%s: %s", operation, strerror(errno));
  caml_failwith(message);
}

CAMLprim value ochat_unix_peer_credentials(value socket_value)
{
  CAMLparam1(socket_value);
  CAMLlocal1(result);
  int socket_fd = Int_val(socket_value);
  uid_t uid;
  gid_t gid;
  int pid = -1;

#if defined(__linux__)
  struct ucred credentials;
  socklen_t length = sizeof(credentials);
  if (getsockopt(socket_fd, SOL_SOCKET, SO_PEERCRED, &credentials, &length) != 0) {
    ochat_peer_fail("getsockopt(SO_PEERCRED)");
  }
  uid = credentials.uid;
  gid = credentials.gid;
  pid = credentials.pid;
#elif defined(__APPLE__) || defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__)
  if (getpeereid(socket_fd, &uid, &gid) != 0) {
    ochat_peer_fail("getpeereid");
  }
#else
  caml_failwith("Unix peer credentials are unavailable on this platform");
#endif

  result = caml_alloc_tuple(3);
  Store_field(result, 0, Val_int(uid));
  Store_field(result, 1, Val_int(gid));
  Store_field(result, 2, Val_int(pid));
  CAMLreturn(result);
}

CAMLprim value ochat_unix_effective_uid(value unit_value)
{
  CAMLparam1(unit_value);
  CAMLreturn(Val_int(geteuid()));
}
