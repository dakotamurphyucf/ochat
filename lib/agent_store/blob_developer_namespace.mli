(** Bounded read-only validation of the one developer blob namespace. The
    caller owns the live Store lease and shared Blob_store retention coordinator;
    this module cannot mint a root or delete a file. *)

val run_ids
  :  reader:Retention_reader.t
  -> directory:string
  -> (Agent_protocol.Id.Developer_run.t list, Store_error.t) result

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
