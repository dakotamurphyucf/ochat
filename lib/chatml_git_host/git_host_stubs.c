#define _GNU_SOURCE
#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <unistd.h>
#ifndef __APPLE__
#include <sys/syscall.h>
#endif

/* Same finite codes as the established private-root policy. */
enum { PS_OK, PS_MISSING, PS_EXISTS, PS_DENIED, PS_UNAVAILABLE,
       PS_UNSUPPORTED, PS_CORRUPT, PS_TOO_LARGE, PS_BUSY };
static int error_code(int e) {
  switch (e) {
  case ENOENT: return PS_MISSING;
  case EEXIST: return PS_EXISTS;
  case EACCES: case EPERM: return PS_DENIED;
  case ELOOP: case ENOTDIR: return PS_CORRUPT;
  case EWOULDBLOCK: return PS_BUSY;
  case ENOSYS: case EOPNOTSUPP: return PS_UNSUPPORTED;
#if ENOTSUP != EOPNOTSUPP
  case ENOTSUP: return PS_UNSUPPORTED;
#endif
  default: return PS_UNAVAILABLE;
  }
}
#include "private_root_checks.h"

struct descriptor { int fd; };
struct cursor { DIR *dir; };
#define Descriptor(v) (*(struct descriptor **)Data_custom_val(v))
#define Cursor(v) (*(struct cursor **)Data_custom_val(v))
static void finalize_descriptor(value v) {
  struct descriptor *p = Descriptor(v);
  if (p) { if (p->fd >= 0) close(p->fd); free(p); }
}
static void finalize_cursor(value v) {
  struct cursor *p = Cursor(v);
  if (p) { if (p->dir) closedir(p->dir); free(p); }
}
static struct custom_operations descriptor_ops = {
  "ochat.git_host.descriptor.v1", finalize_descriptor, custom_compare_default,
  custom_hash_default, custom_serialize_default, custom_deserialize_default,
  custom_compare_ext_default, custom_fixed_length_default
};
static struct custom_operations cursor_ops = {
  "ochat.git_host.cursor.v1", finalize_cursor, custom_compare_default,
  custom_hash_default, custom_serialize_default, custom_deserialize_default,
  custom_compare_ext_default, custom_fixed_length_default
};
CAMLprim value ochat_git_host_descriptor(value unit) {
  CAMLparam1(unit); CAMLlocal1(v);
  v = caml_alloc_custom_mem(&descriptor_ops, sizeof(struct descriptor *), sizeof(struct descriptor));
  Descriptor(v) = NULL;
  struct descriptor *p = malloc(sizeof(*p));
  if (!p) caml_raise_out_of_memory();
  p->fd = -1; Descriptor(v) = p; CAMLreturn(v);
}
CAMLprim value ochat_git_host_cursor(value unit) {
  CAMLparam1(unit); CAMLlocal1(v);
  v = caml_alloc_custom_mem(&cursor_ops, sizeof(struct cursor *), sizeof(struct cursor));
  Cursor(v) = NULL;
  struct cursor *p = malloc(sizeof(*p));
  if (!p) caml_raise_out_of_memory();
  p->dir = NULL; Cursor(v) = p; CAMLreturn(v);
}
CAMLprim value ochat_git_host_close(value owner) {
  CAMLparam1(owner); struct descriptor *p = Descriptor(owner);
  int fd = p->fd; p->fd = -1;
  caml_enter_blocking_section(); if (fd >= 0) close(fd); caml_leave_blocking_section();
  CAMLreturn(Val_unit);
}
CAMLprim value ochat_git_host_close_cursor(value owner) {
  CAMLparam1(owner); struct cursor *p = Cursor(owner);
  DIR *dir = p->dir; p->dir = NULL;
  caml_enter_blocking_section(); if (dir) closedir(dir); caml_leave_blocking_section();
  CAMLreturn(Val_unit);
}
static int component(value name, char out[1025]) {
  mlsize_t n = caml_string_length(name);
  if (!n || n > 1024 || memchr(String_val(name), 0, n) || memchr(String_val(name), '/', n)) return 0;
  memcpy(out, String_val(name), n); out[n] = 0;
  return strcmp(out, ".") && strcmp(out, "..");
}
static int descendant(int fd, int directory) {
  struct stat st;
  if (fstat(fd, &st)) return error_code(errno);
  if ((directory ? !S_ISDIR(st.st_mode) : !S_ISREG(st.st_mode)) ||
      st.st_uid != geteuid() || (st.st_mode & 07002) ||
      (!directory && (st.st_nlink != 1 || (st.st_mode & 0111)))) return PS_CORRUPT;
  return no_acl(fd);
}
CAMLprim value ochat_git_host_root(value owner, value input, value strict) {
  CAMLparam3(owner, input, strict);
  struct descriptor *p = Descriptor(owner); int original = Int_val(input);
  int fd = -1, code = PS_UNAVAILABLE, private = Bool_val(strict);
  if (p->fd < 0) {
    caml_enter_blocking_section();
    fd = fcntl(original, F_DUPFD_CLOEXEC, 0);
    if (fd < 0) code = error_code(errno);
    else if (private) {
      code = private_descriptor(fd, 1, NULL);
      if (code == PS_OK) code = local_filesystem(fd);
    } else {
      struct stat st;
      code = fstat(fd, &st) ? error_code(errno) : (S_ISDIR(st.st_mode) ? PS_OK : PS_CORRUPT);
    }
    if (code != PS_OK && fd >= 0) { close(fd); fd = -1; }
    p->fd = fd; /* Already owned before any raising runtime reentry. */
    caml_leave_blocking_section();
  }
  CAMLreturn(Val_int(code));
}
CAMLprim value ochat_git_host_directory(value owner, value parent, value name, value create, value exclusive) {
  CAMLparam5(owner, parent, name, create, exclusive);
  char path[1025]; int valid = component(name, path);
  struct descriptor *p = Descriptor(owner); int dir = Descriptor(parent)->fd;
  int make = Bool_val(create), excl = Bool_val(exclusive), fd = -1, code = PS_CORRUPT;
  if (valid && dir >= 0 && p->fd < 0) {
    caml_enter_blocking_section();
    code = PS_OK;
    if (make && mkdirat(dir, path, 0700)) {
      if (errno != EEXIST || excl) code = error_code(errno);
    }
    if (code == PS_OK) {
      fd = openat(dir, path, O_RDONLY | O_DIRECTORY | O_NOFOLLOW | O_CLOEXEC);
      code = fd < 0 ? error_code(errno) : descendant(fd, 1);
    }
    if (code != PS_OK && fd >= 0) { close(fd); fd = -1; }
    p->fd = fd; caml_leave_blocking_section();
  }
  CAMLreturn(Val_int(code));
}
CAMLprim value ochat_git_host_file(value owner, value parent, value name, value create) {
  CAMLparam4(owner, parent, name, create);
  char path[1025]; int valid = component(name, path);
  struct descriptor *p = Descriptor(owner); int dir = Descriptor(parent)->fd;
  int make = Bool_val(create), fd = -1, code = PS_CORRUPT;
  if (valid && dir >= 0 && p->fd < 0) {
    caml_enter_blocking_section();
    fd = openat(dir, path, O_NONBLOCK | O_NOFOLLOW | O_CLOEXEC |
                (make ? O_WRONLY | O_CREAT | O_EXCL : O_RDONLY), 0600);
    code = fd < 0 ? error_code(errno) : descendant(fd, 0);
    if (code != PS_OK && fd >= 0) { close(fd); fd = -1; }
    p->fd = fd; caml_leave_blocking_section();
  }
  CAMLreturn(Val_int(code));
}
CAMLprim value ochat_git_host_fd(value owner) {
  CAMLparam1(owner); int fd = Descriptor(owner)->fd;
  if (fd < 0) caml_failwith("closed Git host descriptor");
  CAMLreturn(Val_int(fd));
}
CAMLprim value ochat_git_host_validate_root(value owner) {
  CAMLparam1(owner); int fd = Descriptor(owner)->fd, code;
  caml_enter_blocking_section();
  code = private_descriptor(fd, 1, NULL);
  if (code == PS_OK) code = local_filesystem(fd);
  caml_leave_blocking_section(); CAMLreturn(Val_int(code));
}
CAMLprim value ochat_git_host_lock(value owner) {
  CAMLparam1(owner); int fd = Descriptor(owner)->fd, code;
  caml_enter_blocking_section();
  code = private_descriptor(fd, 0, NULL);
  if (code == PS_OK) code = flock(fd, LOCK_EX | LOCK_NB) ? error_code(errno) : PS_OK;
  caml_leave_blocking_section(); CAMLreturn(Val_int(code));
}
CAMLprim value ochat_git_host_entry(value owner, value name) {
  CAMLparam2(owner, name); CAMLlocal2(result, length);
  char path[1025]; int valid = component(name, path), dir = Descriptor(owner)->fd;
  int code = PS_CORRUPT, kind = 0, fd = -1; struct stat st; memset(&st, 0, sizeof(st));
  if (valid && dir >= 0) {
    caml_enter_blocking_section();
    if (fstatat(dir, path, &st, AT_SYMLINK_NOFOLLOW)) code = error_code(errno);
    else if (S_ISLNK(st.st_mode)) { code = st.st_uid == geteuid() ? PS_OK : PS_CORRUPT; kind = 2; }
    else if (S_ISDIR(st.st_mode) || S_ISREG(st.st_mode)) {
      kind = S_ISDIR(st.st_mode) ? 0 : 1;
      fd = openat(dir, path, O_RDONLY | O_NONBLOCK | O_NOFOLLOW | O_CLOEXEC | (kind == 0 ? O_DIRECTORY : 0));
      code = fd < 0 ? error_code(errno) : descendant(fd, kind == 0);
      struct stat observed;
      if (code == PS_OK && (fstat(fd, &observed) || observed.st_dev != st.st_dev || observed.st_ino != st.st_ino)) code = PS_CORRUPT;
      if (fd >= 0) close(fd);
    } else code = PS_CORRUPT;
    caml_leave_blocking_section();
  }
  length = caml_copy_int64(st.st_size); result = caml_alloc_tuple(3);
  Store_field(result, 0, Val_int(code)); Store_field(result, 1, Val_int(kind)); Store_field(result, 2, length);
  CAMLreturn(result);
}
CAMLprim value ochat_git_host_read(value owner, value buffer, value maximum) {
  CAMLparam3(owner, buffer, maximum);
  int fd = Descriptor(owner)->fd; intnat bytes = Long_val(maximum);
  struct caml_ba_array *array = Caml_ba_array_val(buffer);
  if (bytes < 0 || bytes > array->dim[0]) CAMLreturn(Val_int(-PS_CORRUPT));
  void *data = array->data; ssize_t n;
  caml_enter_blocking_section(); do { n = read(fd, data, bytes); } while (n < 0 && errno == EINTR);
  int code = n < 0 ? error_code(errno) : PS_OK; caml_leave_blocking_section();
  CAMLreturn(Val_long(n < 0 ? -code : n));
}
CAMLprim value ochat_git_host_write(value owner, value buffer, value count) {
  CAMLparam3(owner, buffer, count);
  int fd = Descriptor(owner)->fd; intnat bytes = Long_val(count);
  struct caml_ba_array *array = Caml_ba_array_val(buffer);
  if (bytes < 0 || bytes > array->dim[0]) CAMLreturn(Val_int(-PS_CORRUPT));
  void *data = array->data; ssize_t n;
  caml_enter_blocking_section(); do { n = write(fd, data, bytes); } while (n < 0 && errno == EINTR);
  int code = n < 0 ? error_code(errno) : PS_OK; caml_leave_blocking_section();
  CAMLreturn(Val_long(n < 0 ? -code : n));
}
CAMLprim value ochat_git_host_sync(value owner) {
  CAMLparam1(owner); int fd = Descriptor(owner)->fd, code;
  caml_enter_blocking_section(); code = fsync(fd) ? error_code(errno) : PS_OK;
  caml_leave_blocking_section(); CAMLreturn(Val_int(code));
}
CAMLprim value ochat_git_host_unlink(value owner, value name, value directory) {
  CAMLparam3(owner, name, directory); char path[1025]; int valid = component(name, path);
  int fd = Descriptor(owner)->fd, flags = Bool_val(directory) ? AT_REMOVEDIR : 0, code = PS_CORRUPT;
  if (valid && fd >= 0) { caml_enter_blocking_section(); code = unlinkat(fd, path, flags) ? error_code(errno) : PS_OK; caml_leave_blocking_section(); }
  CAMLreturn(Val_int(code));
}
CAMLprim value ochat_git_host_publish(value parent, value name, value destination, value destination_name) {
  CAMLparam4(parent, name, destination, destination_name);
  char from[1025], to[1025]; int valid = component(name, from) && component(destination_name, to);
  int fd = Descriptor(parent)->fd, out = Descriptor(destination)->fd, code = PS_CORRUPT;
  if (valid && fd >= 0 && out >= 0) {
    caml_enter_blocking_section();
#ifdef __APPLE__
    code = renameatx_np(fd, from, out, to, RENAME_EXCL) ? error_code(errno) : PS_OK;
#elif defined(SYS_renameat2)
    code = syscall(SYS_renameat2, fd, from, out, to, 1 /* RENAME_NOREPLACE */) ? error_code(errno) : PS_OK;
#else
    code = PS_UNSUPPORTED;
#endif
    if (code == PS_OK && (fsync(out) || fsync(fd))) code = PS_UNAVAILABLE;
    caml_leave_blocking_section();
  }
  CAMLreturn(Val_int(code));
}
CAMLprim value ochat_git_host_open_cursor(value owner, value descriptor) {
  CAMLparam2(owner, descriptor); struct cursor *p = Cursor(owner);
  int parent = Descriptor(descriptor)->fd, fd = -1, code = PS_CORRUPT;
  if (!p->dir && parent >= 0) {
    caml_enter_blocking_section();
    fd = openat(parent, ".", O_RDONLY | O_DIRECTORY | O_NOFOLLOW | O_CLOEXEC);
    if (fd < 0) code = error_code(errno);
    else { DIR *dir = fdopendir(fd); if (!dir) { code = error_code(errno); close(fd); } else { p->dir = dir; code = PS_OK; } }
    caml_leave_blocking_section();
  }
  CAMLreturn(Val_int(code));
}
CAMLprim value ochat_git_host_next(value owner, value buffer) {
  CAMLparam2(owner, buffer); DIR *dir = Cursor(owner)->dir;
  struct caml_ba_array *array = Caml_ba_array_val(buffer);
  char *out = array->data; int code = PS_CORRUPT, count = 0;
  if (dir && array->dim[0] >= 1024) {
    caml_enter_blocking_section(); code = PS_OK;
    for (;;) {
      errno = 0; struct dirent *entry = readdir(dir);
      if (!entry) { if (errno) code = error_code(errno); break; }
      if (!strcmp(entry->d_name, ".") || !strcmp(entry->d_name, "..")) continue;
      size_t bytes = strnlen(entry->d_name, 1025);
      if (bytes > 1024) code = PS_TOO_LARGE;
      else { count = (int)bytes; memcpy(out, entry->d_name, bytes); }
      break;
    }
    caml_leave_blocking_section();
  }
  CAMLreturn(Val_int(code == PS_OK ? count : -code));
}
