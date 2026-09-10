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

(** Stable archive filename for a typed reference. A bounded reader must still
    enforce path containment and validate the returned bytes with decode_file. *)
val filename : Session_state.Compaction_archive.t -> string

(** Decode bytes supplied by a bounded reader, using the same checksum, session,
    revision and disposition validation as read, without further filesystem IO. *)
val decode_file
  :  handle:Agent_store.Session_store.Handle.t
  -> max_payload_length:int
  -> Session_state.Compaction_archive.t
  -> string
  -> (Session_state.t, Agent_protocol.Error.t) result
