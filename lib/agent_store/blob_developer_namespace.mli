(** Bounded read-only validation of the one developer blob namespace. The
    caller owns the live Store lease and shared Blob_store retention coordinator;
    this module cannot mint a root or delete a file. *)

val run_ids
  :  reader:Retention_reader.t
  -> directory:string
  -> (Agent_protocol.Id.Developer_run.t list, Store_error.t) result

(** The same canonical namespace enumeration, charging a caller's aggregate
    work allowance during directory streaming and each typed name check. *)
val run_ids_with_checkpoint
  :  reader:Retention_reader.t
  -> checkpoint:(unit -> unit)
  -> directory:string
  -> (Agent_protocol.Id.Developer_run.t list, Store_error.t) result

module Entry : sig
  type t

  module State : sig
    type t =
      | Complete of
          { metadata : Blob_metadata.t
          ; content : string
          }
      | Partial of Blob_metadata.t option
  end

  val id : t -> Agent_protocol.Id.Blob.t
  val state : t -> State.t
  val durable : t -> bool

  (** Original validated metadata document bytes, if present, retained under
      the caller's aggregate Reader allowance. Cross-owner reference absence
      must scan these actual bytes, not only a reserialized typed projection;
      [None] is possible only for a permitted temporary partial fragment. *)
  val metadata_bytes : t -> string option

  (** Decoded metadata string fields, including admitted extension fields and
      escaped values, from the SAME validated document. The caller charges
      iteration/scanning work and does not treat these strings as authority. *)
  val iter_metadata_strings : t -> checkpoint:(unit -> unit) -> f:(string -> unit) -> unit
end

(** Reuse the collector's bounded no-follow namespace scanner for startup
    custody. Every entry/metadata/content byte charges the SAME reader;
    [checkpoint] charges the enclosing work allowance before names, decode,
    copy/publication and each bounded read/hash chunk. Its original exception
    or cancellation propagates. A complete pair has checked target, ID,
    length and digest. [Partial] is permitted only for a TEMPORARY stage
    fragment; the higher intent + real Store job join must match its exact ID
    to an admitted selection before classifying it Incomplete. A partial or
    uncertain DURABLE publication, mismatched metadata, corrupt complete
    content, linked or unknown entry fails the WHOLE scan. The higher join
    compares a Blob ID appearing in temporary and durable locations and
    accepts it only if target, allowed-use, Blob metadata and complete content
    agree; the durable flag reflects its location. Orphan entries fail there.
    Call only for a directory found by a charged parent listing; absence is
    handled by the caller, while a missing directory here fails closed.
    Returned complete bytes remain under the caller's aggregate byte/retention
    cap, not one cap per run. *)
val scan_run
  :  reader:Retention_reader.t
  -> checkpoint:(unit -> unit)
  -> directory:string
  -> target:Blob_metadata.Target.developer
  -> durable:bool
  -> max_blob_bytes:int64
  -> (Entry.t list, Store_error.t) result

(** Reject unknown, linked, incomplete or mismatched metadata/data entries.
    Every listed byte and directory entry charges [reader]; complete pairs are
    retained. A private intent is required before partial stages can enter a
    deletion proof, so partials fail closed here. *)
val validate_run
  :  reader:Retention_reader.t
  -> directory:string
  -> target:Blob_metadata.Target.developer
  -> durable:bool
  -> max_blob_bytes:int64
  -> (unit, Store_error.t) result
