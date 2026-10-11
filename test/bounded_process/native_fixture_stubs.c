#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/resource.h>
#include <sys/mman.h>
#include <unistd.h>
#include <caml/mlvalues.h>

static void write_all(int fd, const char *data, size_t size)
{
  while (size) {
    ssize_t count = write(fd, data, size);
    if (count < 0 && errno == EINTR) continue;
    if (count <= 0) _exit(90);
    data += count;
    size -= (size_t) count;
  }
}

/* Fixed native fixtures, not command evaluation. Blocking code retains the
 * runtime lock; no OCaml pointer is used across an unlocked region or child GC.
 * The forked helper executes C only and cannot return into the OCaml runtime. */
CAMLprim value ochat_process_fixture(value arguments)
{
  if (Wosize_val(arguments) != 2) _exit(91);
  const char *mode = String_val(Field(arguments, 1));
  if (!strcmp(mode, "echo")) {
    char buffer[4096];
    for (;;) {
      ssize_t count = read(0, buffer, sizeof(buffer));
      if (count < 0 && errno == EINTR) continue;
      if (count < 0) _exit(92);
      if (!count) return Val_unit;
      write_all(1, buffer, (size_t) count);
    }
  }
  if (!strcmp(mode, "mixed")) {
    write_all(1, "abc\0d", 5);
    write_all(2, "secret", 6);
    _exit(7);
  }
  if (!strcmp(mode, "signal")) {
    raise(SIGTERM);
    _exit(104);
  }
  if (!strcmp(mode, "kill-anchor")) {
    if (kill(getppid(), SIGKILL)) _exit(105);
    _exit(0);
  }
  if (!strcmp(mode, "reject-input")) {
    write_all(1, "whole", 5);
    _exit(0);
  }
  if (!strcmp(mode, "flood")) {
    char buffer[1024];
    memset(buffer, 'x', sizeof(buffer));
    for (int i = 0; i < 64; i++) {
      write_all(1, buffer, sizeof(buffer));
      write_all(2, buffer, sizeof(buffer));
    }
    _exit(0);
  }
  if (!strcmp(mode, "wait")) {
    for (;;) pause();
  }
  if (!strcmp(mode, "gate-ready")) {
    write_all(1, "command-ready\n", 14);
    for (;;) pause();
  }
  if (!strcmp(mode, "wait-ready")) {
    int fd = open("ready", O_CREAT | O_EXCL | O_WRONLY, 0600);
    if (fd < 0) _exit(101);
    write_all(fd, "!", 1);
    close(fd);
    for (;;) pause();
  }
  if (!strcmp(mode, "held-pipe")) {
    int ready[2];
    if (pipe(ready)) _exit(93);
    pid_t helper = fork();
    if (helper < 0) _exit(94);
    if (!helper) {
      close(ready[0]);
      write_all(1, "helper-ready\n", 13);
      write_all(ready[1], "!", 1);
      close(ready[1]);
      /* This fixture never closes stdout/stderr while alive. EOF independently
         demonstrates helper termination; it does not assert grandchild reap. */
      for (;;) pause();
    }
    close(ready[1]);
    char byte;
    ssize_t count;
    do { count = read(ready[0], &byte, 1); } while (count < 0 && errno == EINTR);
    if (count != 1) _exit(95);
    close(ready[0]);
    write_all(1, "leader-exit\n", 12);
    _exit(0);
  }
  if (!strcmp(mode, "cwd")) {
    int fd = open("marker", O_RDONLY);
    if (fd < 0) _exit(96);
    char buffer[64];
    ssize_t count = read(fd, buffer, sizeof(buffer));
    if (count < 0) _exit(97);
    write_all(1, buffer, (size_t) count);
    close(fd);
    _exit(0);
  }
  if (!strcmp(mode, "limits")) {
    struct rlimit cpu, memory, file, descriptors, core;
    if (getrlimit(RLIMIT_CPU, &cpu) || getrlimit(RLIMIT_AS, &memory)
        || getrlimit(RLIMIT_FSIZE, &file) || getrlimit(RLIMIT_NOFILE, &descriptors)
        || getrlimit(RLIMIT_CORE, &core)) _exit(98);
    char buffer[512];
    int count = snprintf(buffer, sizeof(buffer),
      "group=%d cpu=%llu memory=%llu file=%llu nofile=%llu core=%llu fd3=%d fd4=%d\n",
      getpgrp() == getppid() && getsid(0) == getppid(),
      (unsigned long long) cpu.rlim_cur, (unsigned long long) memory.rlim_cur,
      (unsigned long long) file.rlim_cur, (unsigned long long) descriptors.rlim_cur,
      (unsigned long long) core.rlim_cur, fcntl(3, F_GETFD) >= 0, fcntl(4, F_GETFD) >= 0);
    if (count < 0 || (size_t) count >= sizeof(buffer)) _exit(99);
    write_all(1, buffer, (size_t) count);
    _exit(0);
  }
  if (!strcmp(mode, "memory-bound")) {
    /* One finite reservation of the admitted cap, without touching pages.
       Existing image consumes address space, so a further entire cap must fail. */
    struct rlimit memory;
    if (getrlimit(RLIMIT_AS, &memory) || memory.rlim_cur == RLIM_INFINITY
        || memory.rlim_cur > SIZE_MAX) _exit(102);
    void *mapping = mmap(NULL, (size_t) memory.rlim_cur, PROT_NONE,
                         MAP_PRIVATE | MAP_ANON, -1, 0);
    if (mapping != MAP_FAILED) {
      munmap(mapping, (size_t) memory.rlim_cur);
      _exit(103);
    }
    if (errno != ENOMEM) _exit(102);
    write_all(1, "address-space-bound\n", 20);
    _exit(0);
  }
  _exit(100);
}
