(** Real process fixture using the shared journal-sync crash boundary and a
    deterministic provider. Recovery must execute selected B, then deliver its
    notification; a subsequent reopen must make no provider call. *)
val run_child : Eio_unix.Stdenv.base -> config_path:string -> boundary:string -> unit

(** Parent capture A, edit B, genuine Factory queue admission, SIGKILL before
    launch, delete original child, then two independent daemon reopens. Assert
    exact source/job/owner retention, one execution and no repeat after delivery. *)
val test : Eio_unix.Stdenv.base -> Support.Temporary_environment.t -> unit
