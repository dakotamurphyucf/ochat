(** Independently retained pre-change snapshots. The historical module name and
    references remain compatible with pre-compaction archives.
    References are committed only after the checksummed file and directory
    have been synced. Files remain until their owning session is removed.
    Reference creation admits the encoded child through its typed state decoder
    before certifying the captured document. *)
val reference
  :  Session_state_document.t
  -> limits:Document_schema.Limits.t
  -> Agent_protocol.Id.Operation.t
  -> (Session_state.Compaction_archive.t, Agent_protocol.Error.t) result

(** [reference_for state ~kind id] captures a typed pre-change archive reference.
    References lacking a serialized kind decode as [Compaction]. *)
val reference_for
  :  Session_state_document.t
  -> limits:Document_schema.Limits.t
  -> kind:Session_state.Compaction_archive.kind
  -> Agent_protocol.Id.Operation.t
  -> (Session_state.Compaction_archive.t, Agent_protocol.Error.t) result

val write
  :  env:Eio_unix.Stdenv.base
  -> handle:Agent_store.Session_store.Handle.t
  -> max_payload_length:int
  -> Session_state.Compaction_archive.t
  -> Session_state_document.t
  -> (unit, Agent_protocol.Error.t) result

val read
  :  env:Eio_unix.Stdenv.base
  -> handle:Agent_store.Session_store.Handle.t
  -> max_payload_length:int
  -> Session_state.Compaction_archive.t
  -> (Session_state.t, Agent_protocol.Error.t) result

(** Basename grammar shared by the archive writer and retention reader. Parsing
    admits only known kinds, well-typed operation IDs and the final frame suffix.
    It does not validate the file, authorize access, or admit temporary names. *)
module Name : sig
  type t = private
    { kind : Session_state.Compaction_archive.kind
    ; operation_id : Agent_protocol.Id.Operation.t
    }

  val to_string : t -> string
  val of_string : string -> (t, Agent_protocol.Error.t) result
end

(** Stable archive filename for a typed reference. A bounded reader must still
    enforce path containment and validate the returned bytes with decode_file. *)
val filename : Session_state.Compaction_archive.t -> string

(** Decode bytes supplied by a bounded reader, using the same checksum, session,
    revision and disposition validation as read, without further filesystem IO.
    [charge_work] reserves each operation before processing its known input. *)
val decode_file
  :  ?charge_work:(int -> unit)
  -> handle:Agent_store.Session_store.Handle.t
  -> max_payload_length:int
  -> Session_state.Compaction_archive.t
  -> string
  -> (Session_state.t, Agent_protocol.Error.t) result

(** Validate the original native state, encode and admit its complete typed child.
    Return the original encoded document without reencoding the decoded value. *)
val archive_document
  :  Session_state_document.t
  -> limits:Document_schema.Limits.t
  -> (Document_schema.Document.t, Document_schema.Error.t) result

(** Check the supplied document's bounds, captured digest, typed child, session,
    revision and invocation dispositions before any write. Persist the supplied
    document unchanged, including unknown fields and their order. *)
val write_document
  :  env:Eio_unix.Stdenv.base
  -> handle:Agent_store.Session_store.Handle.t
  -> max_payload_length:int
  -> Session_state.Compaction_archive.t
  -> Document_schema.Document.t
  -> (unit, Agent_protocol.Error.t) result

val decode_record
  :  max_payload_length:int
  -> expected_digest:string option
  -> string
  -> ( Agent_store.Document_record.t * Session_state_document.t
       , Agent_protocol.Error.t )
       result
