(** One exact immutable selection. Intent and commit use the same checksummed
    bytes. A commit lives in its separate, typed publication namespace. *)
open! Core

type t

val reference : t -> Recovery_asset_reference.t
val stage : t -> Blob_stage_documents.t
val contents : t -> string
val max_frame_bytes : int

val filename
  :  Agent_protocol.Recovery_window.Citation.t
  -> part:int
  -> (string, Agent_protocol.Error.t) result

val select
  :  Recovery_asset_reference.t
  -> principal:Agent_protocol.Id.Principal.t
  -> now:Agent_protocol.Timestamp.t
  -> (t, Store_error.t) result

val decode : string -> (t, Store_error.t) result
