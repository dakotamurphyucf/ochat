(** [run env ~config_path ~marker] starts a real daemon and HTTP listener with
    a deterministic model callback requesting [append_to_file]. Pause in the
    actual tool write after the complete marker reaches the filesystem but
    before returning to the tool worker, so no tool-completion acknowledgement
    can occur. The parent must SIGKILL/reap this test-only host. *)
val run : Eio_unix.Stdenv.base -> config_path:string -> marker:string -> unit
