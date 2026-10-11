#ifndef OCHAT_PRIVATE_ROOT_CHECKS_H
#define OCHAT_PRIVATE_ROOT_CHECKS_H
/* Shared native private-root policy. Callers provide the identical finite
   PS_* codes and error_code errno mapping; no ownership or IO lifecycle here. */
#include <errno.h>
#include <sys/stat.h>
#include <unistd.h>
#ifdef __APPLE__
#include <sys/acl.h>
#include <sys/mount.h>
#else
#include <sys/vfs.h>
#include <sys/xattr.h>
#endif
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


#endif
