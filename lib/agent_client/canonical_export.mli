open! Core

(** A complete, fully disclosed export of one exact session revision. This is
    presentation evidence, not permission to seed, edit or execute a session. *)
type t

(** Uses the handle's existing authorized JSON export and authenticated blob
    download. Rejects an advertised size beyond [limits] before downloading;
    validates the complete document under the same finite byte/depth/node/field
    limits before decoding its protocol values. The snapshot's canonical window
    must be complete and entirely Full, and the redundant history must agree
    under [Agent_protocol.Public.History.equal]. Session identity, requested
    revision and returned event sequence must match the export metadata.

    Borrows the handle without another attachment, reader, reducer or lifecycle
    command. No projection/latest-revision fallback is used. Checkpoints bracket
    bounded decoding and comparison stages, whose library internals are not
    preemptible. Existing transport/download operations retain Eio cancellation;
    checkpoint and unexpected exceptions propagate unchanged. Nothing is
    published or written outside private bounded download storage. *)
val read
  :  Session_handle.t
  -> revision:int64
  -> limits:Document_schema.Limits.t
  -> checkpoint:(unit -> unit)
  -> (t, Agent_protocol.Error.t) Result.t

val snapshot : t -> Agent_protocol.Public.Snapshot.t
val metadata : t -> Agent_protocol.Method_result.Export.t
