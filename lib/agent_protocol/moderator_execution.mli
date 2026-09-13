(** Durable execution receipts for ordinary moderator events. These are distinct
    from model operations, tool invocations and conversation deliveries. Records
    confer no authority; the actor owns source/checkpoint admission and execution. *)
type phase =
  | Session_start
  | Session_resume
  | Turn_start
  | Message_appended
  | Pre_tool_call
  | Post_tool_response
  | Turn_end
  | Internal_event
[@@deriving equal, sexp]

type job_attempt =
  { job_id : Id.Job.t
  ; attempt : int
  ; deadline : Timestamp.t option [@sexp.option]
  }
[@@deriving equal, sexp]

type context =
  { id : Id.Moderator_execution.t
  ; session_id : Id.Session.t
  ; generation : int
  ; source : Invocation.observer
  ; operation_id : Id.Operation.t option
  ; job : job_attempt option [@sexp.option]
    (** Background tool event provenance. Mutually exclusive with operation_id;
        only an actor-owned claimed job attempt may admit this context. *)
  ; phase : phase
  ; event : Jsonaf.t
    (** Captured engine event data. The actor validates its encoding and phase
        against the selected event; this pure codec only checks JSON bounds. *)
  ; checkpoint_sha256 : string
  ; created_at : Timestamp.t
  }
[@@deriving equal, sexp]

type status =
  | Running
  | Completed of string
  | Failed of Invocation.tool_error
  | Interrupted of string
[@@deriving equal, sexp]

type intent =
  | Pending
  | Waiting_compaction of Id.Operation.t
  | Applied
  | Discarded of string
[@@deriving equal, sexp]

(** Separate disposition for a consumed failed queue head. The original execution
    outcome remains unchanged. Commit this with the resulting checkpoint. *)
type retirement =
  { checkpoint_sha256 : string
  ; reason : string
  }
[@@deriving equal, sexp]

(** Cross-session provenance for a parent policy check of an already admitted
    child invocation. The host must validate the private delegation and actual
    child invocation; these identifiers grant no authority by themselves. *)
type delegation =
  { child_session_id : Id.Session.t
  ; child_generation : int
  ; child_invocation_id : Id.Invocation.t
  ; admission_sha256 : string
  }
[@@deriving equal, sexp]

module Decision : sig
  type t =
    | Approve
    | Reject of string
    | Rewrite_args of Jsonaf.t
    | Redirect of string * Jsonaf.t
  [@@deriving equal, sexp]

  val validate : t -> (unit, Error.t) result
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

type t = private
  { context : context
  ; status : status
  ; requests : Invocation.follow_up option
  ; intent : intent option
  ; compaction_operation_id : Id.Operation.t option
    (** Retained after application/discard for provenance and recovery checks. *)
  ; retirement : retirement option [@sexp.option]
  ; delegation : delegation option [@sexp.option]
  ; decision : Decision.t option [@sexp.option]
  }
[@@deriving equal, sexp]

val create : context -> (t, Error.t) result

(** Only pre-tool checks without parent operation/job ownership may carry this
    provenance. Completed checks require a decision in the same receipt as the
    committed parent checkpoint. Failed/interrupted checks carry no decision.
    JSON codec4; ordinary/job receipts keep their existing codec2/3 encoding. *)
val create_delegated : delegation:delegation -> context -> (t, Error.t) result

val validate : t -> (unit, Error.t) result

val complete
  :  ?decision:Decision.t
  -> t
  -> checkpoint_sha256:string
  -> requests:Invocation.follow_up
  -> (t, Error.t) result

val fail : t -> Invocation.tool_error -> (t, Error.t) result
val interrupt : t -> reason:string -> (t, Error.t) result
val retire : t -> checkpoint_sha256:string -> reason:string -> (t, Error.t) result

(** Atomically pair intent transitions with their actual actor scheduling or
    stop transition. Waiting_compaction retains the exact dependent operation. *)
val accept_compaction : t -> operation_id:Id.Operation.t -> (t, Error.t) result

val apply_intent : t -> (t, Error.t) result
val discard_intent : t -> reason:string -> (t, Error.t) result
val validate_transition : previous:t option -> t -> (unit, Error.t) result
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
