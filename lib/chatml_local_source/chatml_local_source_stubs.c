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
#include <sys/stat.h>
#include <unistd.h>

/* Match the private finite OCaml Code constructors; never render native text. */
enum { LS_MISSING, LS_DENIED, LS_UNSUPPORTED, LS_KIND, LS_LINK,
       LS_CHANGED, LS_TOO_LARGE, LS_UNAVAILABLE };

struct descriptor { int fd; };
struct stream { DIR *directory; };
#define Descriptor(v) (*(struct descriptor **)Data_custom_val(v))
#define Stream(v) (*(struct stream **)Data_custom_val(v))

static int code_of_errno(int code) {
  switch (code) {
  case ENOENT: return LS_MISSING;
  case EACCES: case EPERM: return LS_DENIED;
  case ELOOP: case ENOTDIR: return LS_LINK;
  case ENOSYS: case EOPNOTSUPP: return LS_UNSUPPORTED;
#if ENOTSUP != EOPNOTSUPP
  case ENOTSUP: return LS_UNSUPPORTED;
#endif
  default: return LS_UNAVAILABLE;
  }
}

static void finalize_descriptor(value owner) {
  struct descriptor *payload = Descriptor(owner);
  if (payload) { if (payload->fd >= 0) close(payload->fd); free(payload); }
}
static void finalize_stream(value owner) {
  struct stream *payload = Stream(owner);
  if (payload) { if (payload->directory) closedir(payload->directory); free(payload); }
}
static struct custom_operations descriptor_ops = {
  "ochat.local_source.descriptor.v1", finalize_descriptor,
  custom_compare_default, custom_hash_default, custom_serialize_default,
  custom_deserialize_default, custom_compare_ext_default, custom_fixed_length_default
};
static struct custom_operations stream_ops = {
  "ochat.local_source.stream.v1", finalize_stream,
  custom_compare_default, custom_hash_default, custom_serialize_default,
  custom_deserialize_default, custom_compare_ext_default, custom_fixed_length_default
};

/* Stable payload allocated before acquisition, rooted by a custom block. The
   OCaml bracket knows this owner before any native acquisition starts. */
CAMLprim value ochat_local_source_create_descriptor(value unit) {
  CAMLparam1(unit); CAMLlocal1(owner);
  owner = caml_alloc_custom_mem(&descriptor_ops, sizeof(struct descriptor *), sizeof(struct descriptor));
  Descriptor(owner) = NULL;
  struct descriptor *payload = malloc(sizeof(struct descriptor));
  if (!payload) caml_raise_out_of_memory();
  payload->fd = -1; Descriptor(owner) = payload;
  CAMLreturn(owner);
}
CAMLprim value ochat_local_source_create_stream(value unit) {
  CAMLparam1(unit); CAMLlocal1(owner);
  owner = caml_alloc_custom_mem(&stream_ops, sizeof(struct stream *), sizeof(struct stream));
  Stream(owner) = NULL;
  struct stream *payload = malloc(sizeof(struct stream));
  if (!payload) caml_raise_out_of_memory();
  payload->directory = NULL; Stream(owner) = payload;
  CAMLreturn(owner);
}
static int copy_component(value name, char output[1025]) {
  mlsize_t length = caml_string_length(name);
  if (length == 0 || length > 1024 || memchr(String_val(name), 0, length) ||
      memchr(String_val(name), '/', length)) return 0;
  memcpy(output, String_val(name), length);
  output[length] = 0;
  return strcmp(output, ".") != 0 && strcmp(output, "..") != 0;
}
static int check_kind(int fd, int directory) {
  struct stat metadata;
  if (fstat(fd, &metadata)) return code_of_errno(errno);
  return (directory ? S_ISDIR(metadata.st_mode) : S_ISREG(metadata.st_mode))
    ? -1 : LS_KIND;
}

CAMLprim value ochat_local_source_duplicate_directory(value owner, value original) {
  CAMLparam2(owner, original); CAMLlocal2(success, failure);
  success = caml_alloc(1, 0); Store_field(success, 0, Val_unit);
  failure = caml_alloc(1, 1); Store_field(failure, 0, Val_int(LS_UNAVAILABLE));
  struct descriptor *payload = Descriptor(owner);
  int original_fd = Int_val(original), fd = -1, code = LS_UNAVAILABLE;
  if (payload->fd < 0) {
    caml_enter_blocking_section();
    fd = fcntl(original_fd, F_DUPFD_CLOEXEC, 0);
    code = fd < 0 ? code_of_errno(errno) : check_kind(fd, 1);
    if (code >= 0 && fd >= 0) { close(fd); fd = -1; }
    payload->fd = fd; /* Stable native memory, BEFORE raising/runtime reentry. */
    caml_leave_blocking_section();
  }
  if (code < 0) CAMLreturn(success);
  Store_field(failure, 0, Val_int(code)); CAMLreturn(failure);
}
static value open_component(value owner, value parent, value name, int directory) {
  CAMLparam3(owner, parent, name); CAMLlocal2(success, failure);
  success = caml_alloc(1, 0); Store_field(success, 0, Val_unit);
  failure = caml_alloc(1, 1); Store_field(failure, 0, Val_int(LS_UNAVAILABLE));
  char component[1025]; int valid = copy_component(name, component);
  struct descriptor *payload = Descriptor(owner);
  int parent_fd = Descriptor(parent)->fd, fd = -1, code = LS_UNAVAILABLE;
  if (valid && parent_fd >= 0 && payload->fd < 0) {
    caml_enter_blocking_section();
    fd = openat(parent_fd, component, O_RDONLY | O_NONBLOCK | O_NOFOLLOW | O_CLOEXEC |
                (directory ? O_DIRECTORY : 0));
    code = fd < 0 ? code_of_errno(errno) : check_kind(fd, directory);
    if (code >= 0 && fd >= 0) { close(fd); fd = -1; }
    payload->fd = fd;
    caml_leave_blocking_section();
  }
  if (code < 0) CAMLreturn(success);
  Store_field(failure, 0, Val_int(code)); CAMLreturn(failure);
}
CAMLprim value ochat_local_source_open_directory(value owner, value parent, value name) {
  return open_component(owner, parent, name, 1);
}
CAMLprim value ochat_local_source_open_file(value owner, value parent, value name) {
  return open_component(owner, parent, name, 0);
}
CAMLprim value ochat_local_source_close_descriptor(value owner) {
  CAMLparam1(owner);
  struct descriptor *payload = Descriptor(owner);
  int fd = payload->fd;
  caml_enter_blocking_section();
  payload->fd = -1; if (fd >= 0) close(fd);
  caml_leave_blocking_section();
  CAMLreturn(Val_unit);
}
CAMLprim value ochat_local_source_open_stream(value owner, value parent) {
  CAMLparam2(owner, parent); CAMLlocal2(success, failure);
  success = caml_alloc(1, 0); Store_field(success, 0, Val_unit);
  failure = caml_alloc(1, 1); Store_field(failure, 0, Val_int(LS_UNAVAILABLE));
  struct stream *payload = Stream(owner);
  int parent_fd = Descriptor(parent)->fd, fd = -1, code = LS_UNAVAILABLE;
  DIR *directory = NULL;
  if (parent_fd >= 0 && !payload->directory) {
    caml_enter_blocking_section();
    fd = openat(parent_fd, ".", O_RDONLY | O_DIRECTORY | O_NOFOLLOW | O_CLOEXEC);
    if (fd < 0) code = code_of_errno(errno);
    else {
      code = check_kind(fd, 1);
      if (code < 0) {
        directory = fdopendir(fd);
        if (!directory) code = code_of_errno(errno);
      }
      if (code >= 0) close(fd);
    }
    payload->directory = directory;
    caml_leave_blocking_section();
  }
  if (code < 0) CAMLreturn(success);
  Store_field(failure, 0, Val_int(code)); CAMLreturn(failure);
}
CAMLprim value ochat_local_source_close_stream(value owner) {
  CAMLparam1(owner);
  struct stream *payload = Stream(owner);
  DIR *directory = payload->directory;
  caml_enter_blocking_section();
  payload->directory = NULL; if (directory) closedir(directory);
  caml_leave_blocking_section();
  CAMLreturn(Val_unit);
}
CAMLprim value ochat_local_source_next_entry(value owner, value buffer) {
  CAMLparam2(owner, buffer);
  DIR *directory = Stream(owner)->directory;
  char *output = Caml_ba_data_val(buffer);
  size_t capacity = Caml_ba_array_val(buffer)->dim[0];
  int result = -(LS_UNAVAILABLE + 1);
  if (directory && capacity >= 1024) {
    caml_enter_blocking_section();
    struct dirent *entry;
    do {
      errno = 0;
      entry = readdir(directory);
    } while (entry && (!strcmp(entry->d_name, ".") || !strcmp(entry->d_name, "..")));
    if (!entry) result = errno ? -(code_of_errno(errno) + 1) : 0;
    else {
      size_t length = strnlen(entry->d_name, 1025);
      if (length == 0 || length > 1024) result = -(LS_KIND + 1);
      else { memcpy(output, entry->d_name, length); result = (int)length; }
    }
    caml_leave_blocking_section();
  }
  CAMLreturn(Val_int(result));
}

CAMLprim value ochat_local_source_observe_file(value owner) {
  CAMLparam1(owner); CAMLlocal3(result, fields, field);
  int fd = Descriptor(owner)->fd, code = LS_UNAVAILABLE;
  struct stat metadata;
  caml_enter_blocking_section();
  if (fd >= 0) {
    if (fstat(fd, &metadata)) code = code_of_errno(errno);
    else code = S_ISREG(metadata.st_mode) ? -1 : LS_KIND;
  }
  caml_leave_blocking_section();
  if (code >= 0) {
    result = caml_alloc(1, 1); Store_field(result, 0, Val_int(code)); CAMLreturn(result);
  }
  int64_t values[7] = { (int64_t)metadata.st_dev, (int64_t)metadata.st_ino,
    (int64_t)metadata.st_size,
#ifdef __APPLE__
    metadata.st_mtimespec.tv_sec, metadata.st_mtimespec.tv_nsec,
    metadata.st_ctimespec.tv_sec, metadata.st_ctimespec.tv_nsec
#else
    metadata.st_mtim.tv_sec, metadata.st_mtim.tv_nsec,
    metadata.st_ctim.tv_sec, metadata.st_ctim.tv_nsec
#endif
  };
  fields = caml_alloc(7, 0);
  for (size_t i = 0; i < 7; ++i) Store_field(fields, i, Val_unit);
  for (size_t i = 0; i < 7; ++i) {
    field = caml_copy_int64(values[i]); Store_field(fields, i, field);
  }
  result = caml_alloc(1, 0); Store_field(result, 0, fields); CAMLreturn(result);
}
CAMLprim value ochat_local_source_same_entry(value parent, value name, value file) {
  CAMLparam3(parent, name, file); CAMLlocal1(result);
  char component[1025]; int valid = copy_component(name, component);
  int parent_fd = Descriptor(parent)->fd, fd = Descriptor(file)->fd, code = LS_CHANGED;
  struct stat opened, current;
  if (valid && parent_fd >= 0 && fd >= 0) {
    caml_enter_blocking_section();
    if (!fstat(fd, &opened) && !fstatat(parent_fd, component, &current, AT_SYMLINK_NOFOLLOW)
        && S_ISREG(current.st_mode) && opened.st_dev == current.st_dev && opened.st_ino == current.st_ino)
      code = -1;
    caml_leave_blocking_section();
  }
  result = caml_alloc(1, code < 0 ? 0 : 1);
  Store_field(result, 0, code < 0 ? Val_unit : Val_int(code)); CAMLreturn(result);
}
CAMLprim value ochat_local_source_read(value owner, value buffer, value requested) {
  CAMLparam3(owner, buffer, requested);
  int fd = Descriptor(owner)->fd, count = Int_val(requested), result = -(LS_UNAVAILABLE + 1);
  size_t capacity = Caml_ba_array_val(buffer)->dim[0];
  char *output = Caml_ba_data_val(buffer);
  if (fd >= 0 && count > 0 && count <= 4096 && (size_t)count <= capacity) {
    caml_enter_blocking_section();
    ssize_t got = read(fd, output, (size_t)count);
    result = got < 0 ? -(code_of_errno(errno) + 1) : (int)got;
    caml_leave_blocking_section();
  }
  CAMLreturn(Val_int(result));
}
