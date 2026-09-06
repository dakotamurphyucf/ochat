(** Independently retained pre-change snapshots. The historical module name and
    references remain compatible with pre-compaction archives.
    References are committed only after the checksummed file and directory
    have been synced. Files remain until their owning session is removed. *)
val reference
  :  Session_state.t
  -> Agent_protocol.Id.Operation.t
  -> Session_state.Compaction_archive.t

(** [reference_for state ~kind id] captures a typed pre-change archive reference.
    References lacking a serialized kind decode as [Compaction]. *)
val reference_for
  :  Session_state.t
  -> kind:Session_state.Compaction_archive.kind
  -> Agent_protocol.Id.Operation.t
  -> Session_state.Compaction_archive.t

val write
  :  env:Eio_unix.Stdenv.base
  -> handle:Agent_store.Session_store.Handle.t
  -> max_payload_length:int
  -> Session_state.Compaction_archive.t
  -> Session_state.t
  -> (unit, Agent_protocol.Error.t) result

val read
  :  env:Eio_unix.Stdenv.base
  -> handle:Agent_store.Session_store.Handle.t
  -> max_payload_length:int
  -> Session_state.Compaction_archive.t
  -> (Session_state.t, Agent_protocol.Error.t) result
