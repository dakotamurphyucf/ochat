(** Actual local TUI catalog/inspection/export and retained selection under an
    isolated durable root. No provider/network request, no legacy migration. *)
val run : Eio_unix.Stdenv.base -> Support.Temporary_environment.t -> unit
