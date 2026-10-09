(** Safe command-center session projection. Explicitly excludes Session.Spec,
    raw errors, prompt/workspace configuration, tool arguments/results and
    operation interruption text. Metadata is intentionally visible. *)
module Observed : sig
  type t =
    | Stopped
    | Queued_for_slot
    | Starting
    | Recovering
    | Idle
    | Running_turn
    | Compacting
    | Waiting_for_permission
    | Stopping
    | Failed
  [@@deriving compare, equal, sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Operation : sig
  (** Exact foreground lifecycle without raw failure/interruption payloads. *)
  module Status : sig
    type t =
      | Starting
      | Running
      | Cancelling
      | Completed
      | Failed
      | Cancelled
      | Interrupted
    [@@deriving compare, equal, sexp]

    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) result
  end

  type t = private
    { id : Id.Operation.t
    ; generation : int
    ; kind : Operation.kind
    ; status : Status.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

type t = private
  { session : Session_ref.t
  ; display_name : string option
  ; labels : (string * string) list
  ; creator : Id.Principal.t option
  ; created_at : Timestamp.t
  ; updated_at : Timestamp.t
  ; generation : int
  ; revision : int64
  ; metadata_revision : int64
  ; latest_event_sequence : int64
  ; execution_host : Session.execution_host
  ; liveness : Session.liveness
  ; persistence : Session.persistence
  ; desired_state : Session.desired_state
  ; observed : Observed.t
  ; archived : bool
  ; effective_organization : Session_organization.Values.t
  ; active_owner_principal_id : Id.Principal.t option
  ; active_operation : Operation.t option
  }
[@@deriving sexp]

(** Copies only named safe fields from the already-authorized checked catalog.
    Validates revisions/generation and metadata structural invariants. *)
val of_catalog : Session_catalog.t -> server_id:Id.Server.t -> (t, Error.t) result

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
