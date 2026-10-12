#include <errno.h>
#include <fcntl.h>
#include <stdint.h>
#include <sys/mman.h>
#include <unistd.h>
#ifdef __APPLE__
#include <libproc.h>
#endif
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>

/* Test-only single bounded measurement, not process enumeration. No allocation,
 * unlocked runtime or mutable parent resource limit. */
CAMLprim value ochat_test_virtual_bytes(value unit)
{
  uint64_t bytes;
#ifdef __APPLE__
  struct proc_taskinfo task;
  int count = proc_pidinfo(getpid(), PROC_PIDTASKINFO, 0, &task, sizeof(task));
  if (count != sizeof(task)) { errno = EIO; caml_uerror("parent VM", Nothing); }
  bytes = task.pti_virtual_size;
#elif defined(__linux__)
  int fd = open("/proc/self/statm", O_RDONLY | O_CLOEXEC);
  if (fd < 0) caml_uerror("parent VM", Nothing);
  char buffer[64];
  ssize_t count;
  do { count = read(fd, buffer, sizeof(buffer)); } while (count < 0 && errno == EINTR);
  int saved = errno;
  close(fd);
  if (count <= 0) { errno = saved; caml_uerror("parent VM", Nothing); }
  uint64_t pages = 0;
  ssize_t i = 0;
  while (i < count && buffer[i] >= '0' && buffer[i] <= '9') {
    unsigned digit = (unsigned) (buffer[i++] - '0');
    if (pages > (UINT64_MAX - digit) / 10) {
      errno = EOVERFLOW; caml_uerror("parent VM", Nothing);
    }
    pages = pages * 10 + digit;
  }
  long page_size = sysconf(_SC_PAGESIZE);
  if (!i || i == count || buffer[i] != ' ' || page_size <= 0
      || pages > UINT64_MAX / (uint64_t) page_size) {
    errno = EOVERFLOW; caml_uerror("parent VM", Nothing);
  }
  bytes = pages * (uint64_t) page_size;
#else
  errno = ENOSYS; caml_uerror("parent VM", Nothing);
#endif
  if (bytes > Max_long) { errno = EOVERFLOW; caml_uerror("parent VM", Nothing); }
  return Val_long(bytes);
}

CAMLprim value ochat_test_mapping_available(value amount)
{
  size_t bytes = (size_t) Long_val(amount);
  void *mapping = mmap(NULL, bytes, PROT_NONE, MAP_PRIVATE | MAP_ANON, -1, 0);
  if (mapping == MAP_FAILED) return Val_false;
  if (munmap(mapping, bytes) < 0) caml_uerror("test munmap", Nothing);
  return Val_true;
}
