#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdint.h>
#include <string.h>
#include <sys/select.h>
#include <sys/wait.h>
#include <unistd.h>
#include "process_anchor.h"
#include "include/fork_action.h"

/* Only forked-child C calls this module. No allocation, OCaml callback, runtime
 * unlock or native pointer retained by the parent. Frames are at most16 bytes. */
static _Noreturn void owner_lost(int group)
{
  kill(group ? 0 : getpid(), SIGKILL);
  _exit(1);                       /* A denied signal cannot imply successful stop. */
}

static void frame(int fd, unsigned char tag, uint32_t status, int group)
{
  unsigned char bytes[16] = { 'O', 'C', 'H', 'G', 1, tag, 0, 0,
    (unsigned char) (status >> 24), (unsigned char) (status >> 16),
    (unsigned char) (status >> 8), (unsigned char) status, 0, 0, 0, 0 };
  size_t offset = 0;
  while (offset < sizeof(bytes)) {
    ssize_t written = write(fd, bytes + offset, sizeof(bytes) - offset);
    if (written < 0 && errno == EINTR) continue;
    if (written <= 0) owner_lost(group);
    offset += (size_t) written;
  }
}

static int read_control(int fd)
{
  unsigned char byte;
  ssize_t count;
  do { count = read(fd, &byte, 1); } while (count < 0 && errno == EINTR);
  return count == 1 && byte == 'G';
}

static _Noreturn void wait_owner(int control, int group)
{
  /* After the sole Go byte, any additional control or EOF is owner loss. */
  (void) read_control(control);
  owner_lost(group);
}

static _Noreturn void report_failure(int status, int control, int group)
{
  frame(status, 4, 0, group);
  wait_owner(control, group);
}

static void child_signal(int signal_number)
{
  (void) signal_number;           /* Native wakeup only; never enter OCaml. */
}

static _Noreturn void command_failed(int errors, char *message)
{
  eio_unix_fork_error(errors, "ochat command setup", message);
  _exit(1);
}

static int blocking(int fd)
{
  int flags = fcntl(fd, F_GETFL);
  return flags >= 0 && fcntl(fd, F_SETFL, flags & ~O_NONBLOCK) == 0;
}

static int command_status(pid_t command, int control, const sigset_t *wait_mask)
{
  for (;;) {
    int status;
    pid_t result;
    do { result = waitpid(command, &status, WNOHANG); }
    while (result < 0 && errno == EINTR);
    if (result == command) return status;
    if (result < 0) return -1;
    fd_set input;
    FD_ZERO(&input);
    FD_SET(control, &input);
    /* SIGCHLD is blocked across the waitpid check. pselect atomically exposes it
     * to a C-only wakeup handler, avoiding a lost exit notification. Owner EOF
     * also wakes this wait while the command is still running. */
    int ready = pselect(control + 1, &input, NULL, NULL, NULL, wait_mask);
    if (ready < 0 && errno == EINTR) continue;
    if (ready > 0) owner_lost(1);
    if (ready < 0) return -1;
  }
}

_Noreturn void ochat_anchor(int errors, value configuration, value actions)
{
  int original_status = Int_val(Field(configuration, 0));
  int original_control = Int_val(Field(configuration, 1));
  /* Keep private endpoints outside Eio's0..2 descriptor mapping. */
  int status = fcntl(original_status, F_DUPFD_CLOEXEC, 5);
  int control = fcntl(original_control, F_DUPFD_CLOEXEC, 5);
  if (status < 0 || control < 0) _exit(1); /* No command exists before Ready/Go. */
  /* Drop parent-only ends before Ready. In particular our inherited control
   * writer must not conceal owner EOF while waiting for Go. Original child ends
   * are also closed now; the executable can inherit only the private duplicates. */
  if (close(Int_val(Field(configuration, 3))) < 0
      || close(Int_val(Field(configuration, 4))) < 0
      || close(original_status) < 0 || close(original_control) < 0) _exit(1);

  sigset_t blocked, original_mask, wait_mask;
  struct sigaction wakeup, original_child_action;
  sigfillset(&blocked);
  if (sigprocmask(SIG_SETMASK, &blocked, &original_mask) < 0) _exit(1);
  memset(&wakeup, 0, sizeof(wakeup));
  wakeup.sa_handler = child_signal;
  sigfillset(&wakeup.sa_mask);
  if (sigaction(SIGCHLD, &wakeup, &original_child_action) < 0) _exit(1);
  if (!blocking(status) || !blocking(control) || !blocking(errors)) _exit(1);
  int group = setsid() >= 0;
  if (!group || ochat_apply_limits(Field(configuration, 2)))
    report_failure(status, control, group);

  frame(status, 1, 0, 1);        /* Ready: group and limits established, no command. */
  if (!read_control(control)) owner_lost(1);
  pid_t command = fork();
  if (command < 0) {
    close(errors);
    report_failure(status, control, 1);
  }
  if (command == 0) {
    if (close(status) < 0 || close(control) < 0)
      command_failed(errors, "cannot close private protocol");
    if (sigaction(SIGCHLD, &original_child_action, NULL) < 0
        || sigprocmask(SIG_SETMASK, &original_mask, NULL) < 0)
      command_failed(errors, "cannot restore command signals");
    eio_unix_run_fork_actions(errors, actions);
    _exit(1);
  }

  /* Failure cannot wait while retaining an error writer: kill the still-live
   * group so parent exec-error EOF and typed unexpected-anchor cleanup progress. */
  if (close(errors) < 0 && errno != EBADF) owner_lost(1);

  /* After command fork, reuse low FD cleanup with only status0/control1 kept.
   * This closes every borrowed cwd/stdio/error writer in the anchor. In particular
   * the existing exec-error EOF must depend on the command, not this live anchor. */
  if (dup2(status, 0) < 0 || dup2(control, 1) < 0)
    report_failure(status, control, 1);
  status = 0;
  control = 1;
  if (ochat_close_extra(status, 1)) report_failure(status, control, 1);
  wait_mask = blocked;
  sigdelset(&wait_mask, SIGCHLD);
  int terminal = command_status(command, control, &wait_mask);
  if (terminal < 0) report_failure(status, control, 1);
  if (WIFEXITED(terminal)) frame(status, 2, (uint32_t) WEXITSTATUS(terminal), 1);
  else if (WIFSIGNALED(terminal)) frame(status, 3, (uint32_t) WTERMSIG(terminal), 1);
  else report_failure(status, control, 1);
  wait_owner(control, 1);        /* Keep group identity live until parent group kill. */
}
