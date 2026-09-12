#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <stddef.h>
#include <stdint.h>
#include <sys/resource.h>
#include <unistd.h>
#ifdef __linux__
#include <sys/syscall.h>
#endif
#ifdef __APPLE__
#include <libproc.h>
#endif
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/unixsupport.h>
#include "include/fork_action.h"

/* Everything after fork runs in C, without allocation or OCaml callbacks. Eio's
 * fork actions provide descriptor mapping and exec; Ochat adds child-only setup. */
static _Noreturn void failed(int errors, char *message)
{
  eio_unix_fork_error(errors, "ochat child setup", message);
  _exit(1);
}

static void close_one(int errors, int fd)
{
  if (close(fd) < 0 && errno != EBADF)
    failed(errors, "descriptor cleanup failed");
}

static void close_extra(int errors)
{
#ifdef __APPLE__
  /* libproc's public wrapper performs only a syscall. Re-query after closing
   * each batch: the kernel may return a prefix when the stack buffer is full.
   * No other thread can add descriptors in this forked child. */
  struct proc_fdinfo descriptors[64];
  for (;;) {
    int bytes = proc_pidinfo(getpid(), PROC_PIDLISTFDS, 0,
                            descriptors, sizeof(descriptors));
    if (bytes <= 0 || bytes > sizeof(descriptors)
        || bytes % sizeof(descriptors[0]) != 0)
      failed(errors, "cannot enumerate child descriptors");
    size_t count = (size_t) bytes / sizeof(descriptors[0]);
    int closed = 0;
    for (size_t i = 0; i < count; i++) {
      int fd = descriptors[i].proc_fd;
      if (fd > 4 && fd != errors) {
        close_one(errors, fd);
        closed++;
      }
    }
    if ((size_t) bytes < sizeof(descriptors)) return;
    if (closed == 0) failed(errors, "descriptor cleanup made no progress");
  }
#elif defined(__linux__)
#ifdef SYS_close_range
  if (errors > 5 && syscall(SYS_close_range, 5U, (unsigned) errors - 1U, 0U) < 0)
    goto enumerate;
  if (syscall(SYS_close_range, (unsigned) errors + 1U, UINT_MAX, 0U) == 0)
    return;
enumerate:;
#endif
  int directory = open("/proc/self/fd", O_RDONLY | O_DIRECTORY | O_CLOEXEC);
  if (directory < 0) failed(errors, "cannot enumerate child descriptors");
  union { uint64_t alignment; char bytes[8192]; } buffer;
  for (;;) {
    struct entry {
      uint64_t inode;
      int64_t offset;
      unsigned short reclen;
      unsigned char type;
      char name[];
    };
    long count = syscall(SYS_getdents64, directory, buffer.bytes, sizeof(buffer.bytes));
    if (count < 0) {
      if (errno == EINTR) continue;
      failed(errors, "cannot read child descriptors");
    }
    if (count == 0) break;
    size_t position = 0;
    while (position < (size_t) count) {
      const size_t prefix = offsetof(struct entry, name);
      if ((size_t) count - position <= prefix) failed(errors, "invalid descriptor directory entry");
      struct entry *entry = (struct entry *) (buffer.bytes + position);
      size_t length = entry->reclen;
      char *name = entry->name;
      if (length <= prefix || length > (size_t) count - position)
        failed(errors, "invalid descriptor directory entry");
      size_t i = 0;
      int fd = 0;
      while (i < length - prefix && name[i] >= '0' && name[i] <= '9') {
        int digit = name[i++] - '0';
        if (fd > (INT_MAX - digit) / 10) failed(errors, "descriptor number overflow");
        fd = fd * 10 + digit;
      }
      if (i > 0 && i < length - prefix && name[i] == '\0'
          && fd > 4 && fd != errors && fd != directory)
        close_one(errors, fd);
      position += length;
    }
  }
  close_one(errors, directory);
#else
  failed(errors, "descriptor cleanup is unsupported on this platform");
#endif
}

static void setup(int errors, value configuration)
{
  if (Bool_val(Field(configuration, 2))) close_extra(errors);
  value limits = Field(configuration, 1);
  while (Is_block(limits)) {
    value pair = Field(limits, 0);
    int resource;
    switch (Int_val(Field(pair, 0))) {
      case 0: resource = RLIMIT_CPU; break;
      case 1:
#ifdef RLIMIT_AS
        resource = RLIMIT_AS; break;
#else
        failed(errors, "virtual memory limits are unsupported");
#endif
      case 2: resource = RLIMIT_FSIZE; break;
      case 3: resource = RLIMIT_NOFILE; break;
      default: failed(errors, "unknown resource limit");
    }
    intnat amount = Long_val(Field(pair, 1));
    rlim_t bound = (rlim_t) amount;
    if (amount < 0 || bound == RLIM_INFINITY || (uintnat) bound != (uintnat) amount)
      failed(errors, "resource limit is not a representable nonnegative bound");
    struct rlimit limit = { .rlim_cur = bound, .rlim_max = bound };
    if (setrlimit(resource, &limit) < 0) failed(errors, "setrlimit failed");
    limits = Field(limits, 1);
  }
}

CAMLprim value ochat_spawn_setup_action(value unit)
{
  return Val_fork_fn(setup);
}

CAMLprim value ochat_spawn(value error_fd, value actions)
{
  CAMLparam2(error_fd, actions);
  pid_t pid = fork();
  if (pid < 0) caml_uerror("fork", Nothing);
  if (pid == 0) {
    /* Keep the error channel outside the supported destination range before
     * Eio's inherit_fds actions can replace those descriptors. */
    int original = Int_val(error_fd);
    int errors = fcntl(original, F_DUPFD_CLOEXEC, 5);
    if (errors < 0) failed(original, "cannot preserve spawn error channel");
    close_one(errors, original);
    eio_unix_run_fork_actions(errors, actions);
    _exit(1);
  }
  CAMLreturn(Val_long(pid));
}
