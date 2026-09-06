(** Mutate only stopped daemon stores, then exercise real child restart and HTTP.
    Keep complete authoritative corruption fail-closed and verify that restoring
    the fixture restores access. Schema-zero/two fixtures test rejection only;
    no implemented historical schema migration exists. *)

val test_journal : Eio_unix.Stdenv.base -> Support.Temporary_environment.t -> unit

val test_snapshot_fallback
  :  Eio_unix.Stdenv.base
  -> Support.Temporary_environment.t
  -> unit

val test_snapshot_corruption
  :  Eio_unix.Stdenv.base
  -> Support.Temporary_environment.t
  -> unit

(** [test_index_missing env environment] requires discovery of the acknowledged
    session after deleting the reconstructable index. Fail rather than bless
    silent session loss when directory-based index rebuilding is unavailable. *)
val test_index_missing : Eio_unix.Stdenv.base -> Support.Temporary_environment.t -> unit

val test_index_corruption
  :  Eio_unix.Stdenv.base
  -> Support.Temporary_environment.t
  -> unit

val test_migration : Eio_unix.Stdenv.base -> Support.Temporary_environment.t -> unit
