(** Process-local approval comparison data. A decoded pending request or ID is
    never a Shell approval, responder grant, Root scope or Worker capability.
    The Host rechecks separate read/respond policy and the physical command or
    claimed Job, source, attempt and origin when using these values. *)

module Owner : sig
  type t =
    | Command of Developer_command_id.t
    | Claimed_job of
        { job_id : Id.Job.t
        ; attempt : int
        }
end

module Execution : sig
  type t =
    | Native_invocation of Id.Invocation.t
    | Claimed_model_job of
        { job_id : Id.Job.t
        ; attempt : int
        }
end

module Pending : sig
  type t

  (** The bounded display is already redacted by the installed owner. No raw
      Shell request or unbounded stdout is represented. An expiration time is
      informational; the real owner clock decides currentness. *)
  val create
    :  limits:Developer_common.Limits.t
    -> id:Id.Permission.t
    -> owner:Owner.t
    -> execution:Execution.t
    -> runtime_id:string
    -> manifest_sha256:string
    -> redacted_display:string
    -> expires_at:Timestamp.t
    -> (t, Error.t) result

  val id : t -> Id.Permission.t
  val owner : t -> Owner.t
  val execution : t -> Execution.t
  val runtime_id : t -> string
  val manifest_sha256 : t -> string
  val redacted_display : t -> string
  val expires_at : t -> Timestamp.t
  val to_json : t -> Jsonaf.t
  val of_json : limits:Developer_common.Limits.t -> Jsonaf.t -> (t, Error.t) result
end

module Page : sig
  type t

  (** One finite process-local snapshot under the advertised page bound. The
      complete JSON also fits [Limits.approval_page]'s bytes, fields, nodes
      and depth. Overflow returns Resource_limit and no partial page; there is
      no cursor or silent truncation. An installation advertises Approval_list
      only when its finite page policy can represent its configured pending
      set. The Host applies current disclosure policy before construction. *)
  val create : limits:Developer_common.Limits.t -> Pending.t list -> (t, Error.t) result

  val pending : t -> Pending.t list
  val to_json : t -> Jsonaf.t
  val of_json : limits:Developer_common.Limits.t -> Jsonaf.t -> (t, Error.t) result
end

module Decision : sig
  type t =
    | Approve_once
    | Deny of string

  (** [Deny] reason is bounded by the installed decision policy. Decoding a
      decision cannot bypass the Host's separate response authorization. *)
  val create_deny : max_reason_bytes:int -> string -> (t, Error.t) result
end

module Response : sig
  type t = private
    { run_id : Id.Developer_run.t
    ; id : Id.Permission.t
    ; decision : Decision.t
    }

  val create
    :  max_reason_bytes:int
    -> run_id:Id.Developer_run.t
    -> id:Id.Permission.t
    -> decision:Decision.t
    -> (t, Error.t) result

  val to_json : t -> Jsonaf.t
  val of_json : max_reason_bytes:int -> Jsonaf.t -> (t, Error.t) result
end
