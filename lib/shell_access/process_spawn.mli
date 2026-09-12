(** Child-only setup integrated into spawning. No companion executable or
    self-execution is involved. Only standard streams and the private channel's
    descriptors (0 through 4) may be inherited through this manager. *)
val manager
  :  cpu_seconds:int option
  -> memory_bytes:int option
  -> file_size_bytes:int option
  -> open_files:int option
  -> close_extra_fds:bool
  -> Eio_unix.Process.mgr_ty Eio.Resource.t
