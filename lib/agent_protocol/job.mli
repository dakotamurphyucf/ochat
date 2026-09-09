(** Durable background job state and control requests. *)

type kind =
  | Model_call
  | Nested_agent
  | Scheduled_event
  | Async_tool
  | Shell_process
  | Compaction
[@@deriving compare, equal, sexp]

(** A saved target invocation returned Pending backed by its own job. The parent
    waits without a worker, retaining its original deadline across restart. *)
type dependency =
  { invocation_id : Id.Invocation.t
  ; job_id : Id.Job.t
  ; deadline : Timestamp.t
  ; completion_schema : Jsonaf.t option [@sexp.option]
    (** Captured from the admitted target definition, never supplied by a script. *)
  ; max_output_bytes : int
  ; max_output_depth : int
  }
[@@deriving equal, sexp]

type status =
  | Queued
  | Running
  | Waiting_permission of Id.Permission.t
  | Waiting_completion of dependency
  | Succeeded
  | Failed of Error.t
  | Cancelled
  | Interrupted of string
[@@deriving sexp]

type retry_policy =
  | Never
  | Safe_retry of
      { max_attempts : int
      ; backoff_ms : int
      }
  | Idempotent of
      { key : Idempotency_key.t
      ; max_attempts : int
      ; backoff_ms : int
      }
[@@deriving sexp]

type delivery =
  | Not_required
  | Pending
  | Delivered of Timestamp.t
[@@deriving sexp]

type launch_owner =
  | Invocation of Id.Invocation.t
  | Moderator_event of Id.Moderator_execution.t
[@@deriving equal, sexp]

type launch =
  { owner : launch_owner
  ; parent_job : (Id.Job.t * int) option [@sexp.option]
  ; nested_depth : int
  }
[@@deriving equal, sexp]

type t =
  { id : Id.Job.t
  ; session_id : Id.Session.t
  ; generation : int
  ; kind : kind
  ; payload : Jsonaf.t
  ; status : status
  ; retry_policy : retry_policy
  ; attempt : int
  ; created_at : Timestamp.t
  ; started_at : Timestamp.t option
  ; next_run_at : Timestamp.t option
  ; completed_at : Timestamp.t option
  ; result : Jsonaf.t option
  ; delivery : delivery
  ; launch : launch option [@sexp.option]
    (** Optional versioned launch provenance. Legacy jobs omit this field.
        Host admission binds the invocation/event owner and actual parent attempt;
        user scripts cannot choose their nesting depth. *)
  }
[@@deriving sexp]

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result

(** Interpret terminal results at the completion/delivery boundary. Async_tool
    results must contain a valid Completion envelope matching their terminal
    status. Other kinds retain the legacy raw-success/error-status encoding;
    JSON that resembles an envelope is still ordinary model output. Nonterminal
    jobs return None, including queued retries retaining an earlier failure.
    This read neither changes delivery ownership nor executes work. *)
val terminal_completion : t -> (Completion.t option, Error.t) result

module List_request : sig
  type t =
    { session_id : Id.Session.t
    ; page : Page.Request.t
    ; status : string option
    ; kind : kind option
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Get_request : sig
  type t =
    { session_id : Id.Session.t
    ; job_id : Id.Job.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Cancel_request : sig
  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; job_id : Id.Job.t
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Cancel_result : sig
  type nonrec t =
    { job : t
    ; mutation : Mutation_result.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end
