(** Durable notification intent. Committing a delivery is only valid in the
    actor transaction that inserts its matching history entry. *)
type source =
  | Moderator
  | Job_adapter
  | External_ingress
[@@deriving compare, equal, sexp]

(** Actual creating moderator source and execution. An absent owner identifies
    a legacy/host-adapter record, never implicit authority for a current script. *)
type ownership =
  { source : Invocation.observer
  ; creator : Job.launch_owner
  }
[@@deriving equal, sexp]

type context =
  { id : Id.Delivery.t
  ; session_id : Id.Session.t
  ; generation : int
  ; invocation_id : Id.Invocation.t option
  ; work : Invocation.work option
  ; correlation : string
  ; source : source
  ; completion : Completion.t
  ; wake : Completion.wake
  ; created_at : Timestamp.t
  ; ownership : ownership option [@sexp.option]
  }
[@@deriving equal, sexp]

type status =
  | Pending
  | Committed of
      { history_id : History_entry.Id.t
      ; at : Timestamp.t
      }
  | Failed of Invocation.tool_error
[@@deriving equal, sexp]

(** A durable request to wake after history insertion. Accepted binds the actual
    foreground operation admitted by the actor; it does not claim model success.
    Discarded retains a bounded explanation for policy/lifecycle rejection. *)
type wake_disposition =
  | Pending_wake
  | Accepted_wake of Id.Operation.t
  | Discarded_wake of string
[@@deriving equal, sexp]

type t = private
  { context : context
  ; attempt : int
  ; status : status
  ; wake_disposition : wake_disposition option [@sexp.option]
    (** Envelope3, or Envelope4 with disclosure pins. Only committed Request_turn deliveries carry
        this receipt. Source ownership is independent of wake tracking.
        Historical absent values do not acquire a new wake. *)
  ; disclosure_pins : (string * string) list option [@sexp.option]
    (** Envelope4: immutable ordered configuration pins for the publisher's exact
        tool ceiling. None is historical/untracked, not authority to use the whole
        current registry. Some [] is an explicitly empty ceiling. *)
  ; completion_projection : Completion_projection.t option [@sexp.option]
    (** Envelope5: immutable original-result evidence for a standalone adapter.
        The host validates the contract and actual job before admission. *)
  }
[@@deriving equal, sexp]

val create
  :  ?disclosure_pins:(string * string) list
  -> ?completion_projection:Completion_projection.t
  -> context
  -> (t, Error.t) result

val validate : t -> (unit, Error.t) result

(** New execution services opt into durable wake tracking with [track_wake:true].
    It creates Pending_wake only for Request_turn, for moderators or approved host
    adapters. The default preserves legacy publication behavior; reading or
    recommitting an existing record never synthesizes a new wake. *)
val commit
  :  ?track_wake:bool
  -> t
  -> history_id:History_entry.Id.t
  -> now:Timestamp.t
  -> (t, Error.t) result

(** Settle once, idempotently for the same disposition. The host must commit
    acceptance with admission of the named operation, using Delivery_wake_changed;
    these pure transitions alone neither authorize nor start a turn. *)
val accept_wake : t -> operation_id:Id.Operation.t -> (t, Error.t) result

val discard_wake : t -> reason:string -> (t, Error.t) result
val fail : t -> Invocation.tool_error -> (t, Error.t) result
val retry : t -> max_attempts:int -> (t, Error.t) result
val validate_transition : previous:t option -> t -> (unit, Error.t) result
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
