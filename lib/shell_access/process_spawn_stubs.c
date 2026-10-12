#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <stddef.h>
#include <stdint.h>
#include <sys/resource.h>
#include <sys/wait.h>
#include <signal.h>
#include <string.h>
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
#include "ocaml_utils.h"  /* Installed Jane Street signal normalization declaration. */
#include "process_anchor.h"
#include "include/fork_action.h"

/* Everything after fork runs in C, without allocation or OCaml callbacks. Eio's
 * fork actions provide descriptor mapping and exec; Ochat adds child-only setup. */
static _Noreturn void failed(int errors, char *message)
{
  eio_unix_fork_error(errors, "ochat child setup", message);
  _exit(1);
}

static const char *close_one(int fd)
{
  if (close(fd) < 0 && errno != EBADF)
    return "descriptor cleanup failed";
  return NULL;
}

const char *ochat_close_extra(int errors, int highest)
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
      return "cannot enumerate child descriptors";
    size_t count = (size_t) bytes / sizeof(descriptors[0]);
    int closed = 0;
    for (size_t i = 0; i < count; i++) {
      int fd = descriptors[i].proc_fd;
      if (fd > highest && fd != errors) {
        const char *error = close_one(fd);
        if (error) return error;
        closed++;
      }
    }
    if ((size_t) bytes < sizeof(descriptors)) return NULL;
    if (closed == 0) return "descriptor cleanup made no progress";
  }
#elif defined(__linux__)
#ifdef SYS_close_range
  unsigned first = (unsigned) highest + 1U;
  if ((unsigned) errors > first && syscall(SYS_close_range, first, (unsigned) errors - 1U, 0U) < 0)
    goto enumerate;
  unsigned after_error = (unsigned) errors + 1U;
  if (after_error < first) after_error = first;
  if (syscall(SYS_close_range, after_error, UINT_MAX, 0U) == 0)
    return NULL;
enumerate:;
#endif
  int directory = open("/proc/self/fd", O_RDONLY | O_DIRECTORY | O_CLOEXEC);
  if (directory < 0) return "cannot enumerate child descriptors";
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
      return "cannot read child descriptors";
    }
    if (count == 0) break;
    size_t position = 0;
    while (position < (size_t) count) {
      const size_t prefix = offsetof(struct entry, name);
      if ((size_t) count - position <= prefix) return "invalid descriptor directory entry";
      struct entry *entry = (struct entry *) (buffer.bytes + position);
      size_t length = entry->reclen;
      char *name = entry->name;
      if (length <= prefix || length > (size_t) count - position)
        return "invalid descriptor directory entry";
      size_t i = 0;
      int fd = 0;
      while (i < length - prefix && name[i] >= '0' && name[i] <= '9') {
        int digit = name[i++] - '0';
        if (fd > (INT_MAX - digit) / 10) return "descriptor number overflow";
        fd = fd * 10 + digit;
      }
      if (i > 0 && i < length - prefix && name[i] == '\0'
          && fd > highest && fd != errors && fd != directory)
      {
        const char *error = close_one(fd);
        if (error) return error;
      }
      position += length;
    }
  }
  return close_one(directory);
#else
  return "descriptor cleanup is unsupported on this platform";
#endif
}

static void setup(int errors, value configuration)
{
  int group = Bool_val(Field(configuration, 3));
  if (Bool_val(Field(configuration, 2))) {
    const char *error = ochat_close_extra(errors, group ? 2 : 4);
    if (error) failed(errors, (char *) error);
  }
  const char *error = ochat_apply_limits(Field(configuration, 1));
  if (error) failed(errors, (char *) error);
}

const char *ochat_apply_limits(value limits)
{
  while (Is_block(limits)) {
    value pair = Field(limits, 0);
    int resource;
    switch (Int_val(Field(pair, 0))) {
      case 0: resource = RLIMIT_CPU; break;
      case 1:
#ifdef RLIMIT_AS
        resource = RLIMIT_AS; break;
#else
        return "virtual memory limits are unsupported";
#endif
      case 2: resource = RLIMIT_FSIZE; break;
      case 3: resource = RLIMIT_NOFILE; break;
      case 4: resource = RLIMIT_CORE; break;
      default: return "unknown resource limit";
    }
    intnat amount = Long_val(Field(pair, 1));
    rlim_t bound = (rlim_t) amount;
    if (amount < 0 || bound == RLIM_INFINITY || (uintnat) bound != (uintnat) amount)
      return "resource limit is not a representable nonnegative bound";
    struct rlimit limit = { .rlim_cur = bound, .rlim_max = bound };
    if (setrlimit(resource, &limit) < 0) {
      switch (Int_val(Field(pair, 0))) {
        case 0: return "CPU setrlimit failed";
        case 1: return "memory setrlimit failed";
        case 2: return "file setrlimit failed";
        case 3: return "descriptor setrlimit failed";
        case 4: return "core setrlimit failed";
        default: return "setrlimit failed";
      }
    }
    limits = Field(limits, 1);
  }
  return NULL;
}

CAMLprim value ochat_owned_group_supported(value unit)
{
#if (defined(__APPLE__) || defined(__linux__)) && defined(WNOWAIT) && defined(RLIMIT_AS)
  return Val_true;
#else
  return Val_false;
#endif
}

/* Nonblocking and deliberately does not release the child's identity. No
 * runtime unlock: all arguments are immediates and no heap pointer escapes. */
CAMLprim value ochat_child_exited_without_reaping(value child_pid)
{
#if (defined(__APPLE__) || defined(__linux__)) && defined(WNOWAIT)
  siginfo_t status;
  int result;
  do {
    memset(&status, 0, sizeof(status));
    result = waitid(P_PID, (id_t) Long_val(child_pid), &status,
                    WEXITED | WNOHANG | WNOWAIT);
  } while (result < 0 && errno == EINTR);
  if (result < 0) caml_uerror("waitid", Nothing);
  return Val_bool(status.si_pid != 0);
#else
  errno = ENOSYS;
  caml_uerror("waitid", Nothing);
#endif
}

CAMLprim value ochat_spawn_setup_action(value unit)
{
  return Val_fork_fn(setup);
}

CAMLprim value ochat_normalize_signal(value signal_number)
{
  return Val_int(caml_rev_convert_signal_number(Int_val(signal_number)));
}

CAMLprim value ochat_spawn(value error_fd, value owned, value actions)
{
  /* All action/configuration values are rooted in the parent until fork returns.
   * No runtime unlock, child OCaml allocation, callback or heap-pointer escape. */
  CAMLparam3(error_fd, owned, actions);
  pid_t pid = fork();
  if (pid < 0) caml_uerror("fork", Nothing);
  if (pid == 0) {
    int original = Int_val(error_fd);
    int errors = fcntl(original, F_DUPFD_CLOEXEC, 5);
    if (errors < 0) failed(original, "cannot preserve spawn error channel");
    const char *error = close_one(original);
    if (error) failed(errors, (char *) error);
    if (Is_block(owned))
      ochat_anchor(errors, Field(owned, 0), actions);
    eio_unix_run_fork_actions(errors, actions);
    _exit(1);
  }
  CAMLreturn(Val_long(pid));
}
