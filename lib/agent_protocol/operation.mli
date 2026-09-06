(** Foreground operation metadata shared by transports and clients. *)

type turn_start_reason =
  | User_submit
  | Moderator_request
  | Idle_followup
  | Recovery_retry
  | Administrative
[@@deriving compare, equal, sexp]

type kind =
  | Turn of turn_start_reason
  | Compaction
[@@deriving compare, equal, sexp]

type state =
  | Starting
  | Running
  | Cancelling
  | Completed
  | Failed of Error.t
  | Cancelled
  | Interrupted of
      { reason : string
      ; retryable : bool
      }
[@@deriving sexp]

type t =
  { id : Id.Operation.t
  ; generation : int
  ; kind : kind
  ; state : state
  ; started_at : Timestamp.t
  ; updated_at : Timestamp.t
  }
[@@deriving sexp]

(** [to_json t] encodes an operation summary. *)
val to_json : t -> Jsonaf.t

(** [of_json json] decodes and validates an operation summary. *)
val of_json : Jsonaf.t -> (t, Error.t) result
