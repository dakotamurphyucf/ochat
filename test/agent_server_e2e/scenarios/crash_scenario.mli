open Core

(** Run opt-in E2E-22 process crashes using actual Eio writes, syncs and renames.
    Private [child.*] cases use [OCHAT_E2E_CRASH_ARGUMENTS]; no shared dispatcher
    changes are needed. SIGKILL does not simulate power loss or volatile caches. *)
val run : Eio_unix.Stdenv.base -> case:string option -> unit

(** [run_child env arguments] accepts [["replace"; boundary; absolute_target]]
    or [["journal"; byte_count; absolute_directory]]. Also accept
    [["side-effect"; config_path; marker]] for the scripted daemon/tool host.
    Report readiness/boundary markers on stdout and await external termination
    at the selected IO boundary. Replacement boundaries include exclusive
    temporary-file creation before the first write and native directory sync.
    [after-directory-sync] pauses only after observing the native directory open
    and a successful return from the production durable replacement. *)
val run_child : Eio_unix.Stdenv.base -> string list -> unit
