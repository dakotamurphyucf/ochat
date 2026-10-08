#define _GNU_SOURCE
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>
#include <errno.h>
#include <fcntl.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <unistd.h>
#ifdef __APPLE__
#include <sys/acl.h>
#include <sys/mount.h>
#else
#include <sys/vfs.h>
#include <sys/xattr.h>
#include <sys/syscall.h>
#endif

/* Finite codes only. Native diagnostics must never render names or contents. */
enum { PS_OK, PS_MISSING, PS_EXISTS, PS_DENIED, PS_UNAVAILABLE,
       PS_UNSUPPORTED, PS_CORRUPT, PS_TOO_LARGE, PS_BUSY };

/* Volatile stores retain clearing on SDKs without explicit_bzero. */
static void clear_buffer(void *buffer, size_t length) {
  volatile unsigned char *bytes = buffer;
  while (length--) *bytes++ = 0;
}

static int random_bytes(void *buffer, size_t length) {
#ifdef __APPLE__
  arc4random_buf(buffer, length);
  return 0;
#else
  return getentropy(buffer, length);
#endif
}

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

static int no_acl(int fd) {
#ifdef __APPLE__
  acl_t acl = acl_get_fd_np(fd, ACL_TYPE_EXTENDED);
  /* Darwin's descriptor-backed FILESEC_ACL lookup reports ENOENT when no
     extended ACL property exists. Other failures remain inadmissible. */
  if (!acl) return errno == ENOENT ? PS_OK : PS_UNSUPPORTED;
  if (acl_valid(acl) != 0) { acl_free(acl); return PS_CORRUPT; }
  acl_entry_t entry;
  /* Darwin returns zero for an existing entry, unlike Linux's POSIX ACL API.
     For this validated ACL, FIRST_ENTRY/EINVAL means the ACL is empty. */
  int status = acl_get_entry(acl, ACL_FIRST_ENTRY, &entry);
  int saved_errno = errno;
  acl_free(acl);
  if (status == 0) return PS_CORRUPT;
  return status == -1 && saved_errno == EINVAL ? PS_OK : PS_UNSUPPORTED;
#else
  const char *names[] = { "system.posix_acl_access", "system.posix_acl_default" };
  for (size_t i = 0; i < sizeof(names) / sizeof(names[0]); i++) {
    ssize_t size = fgetxattr(fd, names[i], NULL, 0);
    if (size > 0) return PS_CORRUPT;
    if (size < 0 && errno != ENODATA) return PS_UNSUPPORTED;
  }
  return PS_OK;
#endif
}

static int local_filesystem(int fd) {
  struct statfs fs;
  if (fstatfs(fd, &fs) != 0) return error_code(errno);
#ifdef __APPLE__
  return (fs.f_flags & MNT_LOCAL) ? PS_OK : PS_UNSUPPORTED;
#else
  /* Explicit local implementations, not a default acceptance of unknown FS. */
  switch ((unsigned long)fs.f_type) {
  case 0xef53: case 0x58465342: case 0x9123683e: case 0x01021994:
  case 0x794c7630: return PS_OK; /* ext, xfs, btrfs, tmpfs, overlay */
  default: return PS_UNSUPPORTED;
  }
#endif
}

static int private_descriptor(int fd, int directory, struct stat *out) {
  struct stat st;
  if (fstat(fd, &st) != 0) return error_code(errno);
  if ((directory ? !S_ISDIR(st.st_mode) : !S_ISREG(st.st_mode)) ||
      st.st_uid != geteuid() ||
      (st.st_mode & 07777) != (directory ? 0700 : 0600) ||
      (!directory && st.st_nlink != 1)) return PS_CORRUPT;
  int code = no_acl(fd);
  if (code == PS_OK && out) *out = st;
  return code;
}

static int same_inode(int dir, const char *name, int fd) {
  struct stat first, current;
  if (fstat(fd, &first) || fstatat(dir, name, &current, AT_SYMLINK_NOFOLLOW))
    return 0;
  return first.st_dev == current.st_dev && first.st_ino == current.st_ino;
}

static int validate_directory(int fd) {
  int code = private_descriptor(fd, 1, NULL);
  return code == PS_OK ? local_filesystem(fd) : code;
}

static int open_private_file(int dir, const char *name, int create, int *output) {
  /* Nonblocking admission prevents FIFO/device hangs before fstat rejection. */
  int fd = openat(dir, name, O_RDONLY | O_NONBLOCK | O_NOFOLLOW | O_CLOEXEC |
                  (create ? O_CREAT : 0), 0600);
  if (fd < 0) return error_code(errno);
  int code = private_descriptor(fd, 0, NULL);
  if (code != PS_OK) { close(fd); return code; }
  *output = fd;
  return PS_OK;
}

static value tuple3(int code, int flag, int fd) {
  CAMLparam0(); CAMLlocal1(result);
  result = caml_alloc_tuple(3);
  Store_field(result, 0, Val_int(code));
  Store_field(result, 1, Val_bool(flag));
  Store_field(result, 2, Val_int(fd));
  CAMLreturn(result);
}

CAMLprim value ochat_private_open_directory(value anchor, value components) {
  CAMLparam2(anchor, components);
  size_t count = Wosize_val(components);
  int anchor_fd = Int_val(anchor);
  char **names = calloc(count, sizeof(char *));
  int code = PS_UNAVAILABLE, fd = -1, published = 0;
  if (names) {
    size_t copied = 0;
    for (; copied < count; copied++) {
      names[copied] = strdup(String_val(Field(components, copied)));
      if (!names[copied]) break;
    }
    if (copied == count) {
      caml_enter_blocking_section();
      fd = fcntl(anchor_fd, F_DUPFD_CLOEXEC, 0);
      code = fd < 0 ? error_code(errno) : local_filesystem(fd);
      for (size_t i = 0; code == PS_OK && i < count; i++) {
        int next = openat(fd, names[i], O_RDONLY | O_DIRECTORY | O_NOFOLLOW | O_CLOEXEC);
        if (next < 0 && errno == ENOENT) {
          if (mkdirat(fd, names[i], 0700) == 0) {
            published = 1;
            if (fsync(fd) != 0) { code = error_code(errno); break; }
          } else if (errno != EEXIST) { code = error_code(errno); break; }
          next = openat(fd, names[i], O_RDONLY | O_DIRECTORY | O_NOFOLLOW | O_CLOEXEC);
        }
        if (next < 0) { code = error_code(errno); break; }
        code = private_descriptor(next, 1, NULL);
        if (code == PS_OK) code = local_filesystem(next);
        close(fd); fd = next;
      }
      if (code != PS_OK && fd >= 0) { close(fd); fd = -1; }
      caml_leave_blocking_section();
    }
    for (size_t i = 0; i < count; i++) free(names[i]);
  }
  free(names);
  CAMLreturn(tuple3(code, published, fd));
}

CAMLprim value ochat_private_read(value directory, value name, value maximum) {
  CAMLparam3(directory, name, maximum); CAMLlocal2(result, contents);
  int dir = Int_val(directory), fd = -1, code = PS_UNAVAILABLE;
  size_t cap = Long_val(maximum), length = 0;
  char *filename = strdup(String_val(name));
  char *buffer = malloc(cap + 1);
  if (filename && buffer) {
    caml_enter_blocking_section();
    code = validate_directory(dir);
    if (code == PS_OK) code = open_private_file(dir, filename, 0, &fd);
    struct stat st;
    if (code == PS_OK) {
      if (fstat(fd, &st)) code = error_code(errno);
      else if (st.st_size < 0 || (uint64_t)st.st_size > cap) code = PS_TOO_LARGE;
    }
    while (code == PS_OK) {
      ssize_t n = read(fd, buffer + length, cap + 1 - length);
      if (n < 0) { if (errno == EINTR) continue; code = error_code(errno); break; }
      if (n == 0) break;
      length += (size_t)n;
      if (length > cap) { code = PS_TOO_LARGE; break; }
    }
    if (fd >= 0) close(fd);
    caml_leave_blocking_section();
  }
  contents = caml_alloc_string(code == PS_OK ? length : 0);
  if (code == PS_OK && length) memcpy(Bytes_val(contents), buffer, length);
  if (buffer) { clear_buffer(buffer, cap + 1); free(buffer); }
  free(filename);
  result = caml_alloc_tuple(2);
  Store_field(result, 0, Val_int(code)); Store_field(result, 1, contents);
  CAMLreturn(result);
}

static int publish(int dir, const char *temp, const char *target, int replace) {
  if (replace) return renameat(dir, temp, dir, target);
#ifdef __APPLE__
  return renameatx_np(dir, temp, dir, target, RENAME_EXCL);
#elif defined(SYS_renameat2)
  return syscall(SYS_renameat2, dir, temp, dir, target, 1 /* RENAME_NOREPLACE */);
#else
  errno = ENOSYS; return -1;
#endif
}

/* fault: 1 before publication; 2 after publication before directory sync. */
CAMLprim value ochat_private_write(value directory, value name, value data,
                                   value replace_value, value fault_value) {
  CAMLparam5(directory, name, data, replace_value, fault_value);
  int dir = Int_val(directory), replace = Bool_val(replace_value);
  int fault = Int_val(fault_value), code = PS_UNAVAILABLE, fd = -1, published = 0;
  size_t length = caml_string_length(data);
  char *filename = strdup(String_val(name)), *buffer = malloc(length ? length : 1);
  if (buffer && length) memcpy(buffer, String_val(data), length);
  char temp[80] = {0}; int temp_owned = 0;
  if (filename && buffer) {
    caml_enter_blocking_section();
    int existing = -1;
    code = validate_directory(dir);
    if (code == PS_OK) {
      code = open_private_file(dir, filename, 0, &existing);
      if (code == PS_OK) { close(existing); if (!replace) code = PS_EXISTS; }
      else if (code == PS_MISSING) code = PS_OK;
    }
    if (code == PS_OK) {
      unsigned char random[16];
      if (random_bytes(random, sizeof(random)) != 0) code = error_code(errno);
      else {
        memcpy(temp, ".ochat-private-", 15);
        static const char hex[] = "0123456789abcdef";
        for (int i = 0; i < 16; i++) { temp[15 + 2*i] = hex[random[i] >> 4]; temp[16 + 2*i] = hex[random[i] & 15]; }
        fd = openat(dir, temp, O_WRONLY | O_CREAT | O_EXCL | O_NOFOLLOW | O_CLOEXEC, 0600);
        if (fd < 0) code = error_code(errno);
        else { temp_owned = 1; code = private_descriptor(fd, 0, NULL); }
      }
    }
    size_t offset = 0;
    while (code == PS_OK && offset < length) {
      ssize_t n = write(fd, buffer + offset, length - offset);
      if (n < 0) { if (errno == EINTR) continue; code = error_code(errno); break; }
      if (n == 0) { code = PS_UNAVAILABLE; break; }
      offset += (size_t)n;
    }
    if (code == PS_OK && fsync(fd) != 0) code = error_code(errno);
    if (code == PS_OK && fault == 1) code = PS_UNAVAILABLE;
    if (code == PS_OK) {
      if (publish(dir, temp, filename, replace) != 0) code = error_code(errno);
      else { published = 1; temp_owned = 0; }
    }
    if (code == PS_OK && fault == 2) code = PS_UNAVAILABLE;
    if (code == PS_OK && fsync(dir) != 0) code = error_code(errno);
    if (temp_owned) unlinkat(dir, temp, 0);
    if (!published && fd >= 0) { close(fd); fd = -1; }
    caml_leave_blocking_section();
  }
  if (buffer) { clear_buffer(buffer, length); free(buffer); }
  free(filename);
  CAMLreturn(tuple3(code, published, fd));
}

CAMLprim value ochat_private_confirm_absent(value directory, value name, value fail_sync) {
  CAMLparam3(directory, name, fail_sync);
  int dir = Int_val(directory), inject_failure = Bool_val(fail_sync), code = PS_UNAVAILABLE;
  char *filename = strdup(String_val(name));
  if (filename) {
    caml_enter_blocking_section();
    code = validate_directory(dir);
    if (code == PS_OK) {
      struct stat entry;
      if (fstatat(dir, filename, &entry, AT_SYMLINK_NOFOLLOW) == 0) code = PS_EXISTS;
      else if (errno == ENOENT) {
        if (inject_failure) code = PS_UNAVAILABLE;
        else if (fsync(dir) != 0) code = error_code(errno);
      } else code = error_code(errno);
    }
    caml_leave_blocking_section();
  }
  free(filename);
  CAMLreturn(Val_int(code));
}

CAMLprim value ochat_private_delete(value directory, value name, value owned, value check_owned) {
  CAMLparam4(directory, name, owned, check_owned); CAMLlocal1(result);
  int dir = Int_val(directory), expected = Int_val(owned), check = Bool_val(check_owned), fd = -1, removed = 0;
  char *filename = strdup(String_val(name)); int code = PS_UNAVAILABLE;
  if (filename) {
    caml_enter_blocking_section();
    code = validate_directory(dir);
    if (code == PS_OK) code = open_private_file(dir, filename, 0, &fd);
    if (code == PS_OK && (!same_inode(dir, filename, fd) ||
        (check && !same_inode(dir, filename, expected)))) code = PS_CORRUPT;
    if (code == PS_OK) {
      if (unlinkat(dir, filename, 0) != 0) code = error_code(errno);
      else { removed = 1; if (fsync(dir) != 0) code = error_code(errno); }
    }
    if (fd >= 0) close(fd);
    caml_leave_blocking_section();
  }
  free(filename); result = caml_alloc_tuple(2);
  Store_field(result, 0, Val_int(code)); Store_field(result, 1, Val_bool(removed));
  CAMLreturn(result);
}

CAMLprim value ochat_private_lock(value directory, value name, value exclusive) {
  CAMLparam3(directory, name, exclusive); CAMLlocal1(result);
  int dir = Int_val(directory), fd = -1, code = PS_UNAVAILABLE, created = 0;
  int is_exclusive = Bool_val(exclusive);
  char *filename = strdup(String_val(name));
  if (filename) {
    caml_enter_blocking_section();
    code = validate_directory(dir);
    if (code == PS_OK) {
      fd = openat(dir, filename, O_RDONLY | O_CREAT | O_EXCL | O_NOFOLLOW | O_NONBLOCK | O_CLOEXEC, 0600);
      if (fd >= 0) { created = 1; code = private_descriptor(fd, 0, NULL); }
      else if (errno == EEXIST) code = open_private_file(dir, filename, 0, &fd);
      else code = error_code(errno);
    }
    if (code == PS_OK && (fsync(fd) != 0 || fsync(dir) != 0)) code = error_code(errno);
    if (code == PS_OK && flock(fd, (is_exclusive ? LOCK_EX : LOCK_SH) | LOCK_NB) != 0)
      code = error_code(errno);
    if (code != PS_OK && fd >= 0) { close(fd); fd = -1; }
    caml_leave_blocking_section();
  }
  free(filename); result = tuple3(code, created, fd);
  CAMLreturn(result);
}

CAMLprim value ochat_private_close(value descriptor) {
  CAMLparam1(descriptor);
  int fd = Int_val(descriptor);
  caml_enter_blocking_section(); close(fd); caml_leave_blocking_section();
  CAMLreturn(Val_unit);
}
