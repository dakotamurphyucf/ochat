(** Exact method/result affinity for the portable Developer family. This is
    codec inventory, not evidence that a Host installed or selected a handler.
    A client additionally checks Initialize features, actual Capabilities and
    response method identity; an authenticated server reauthorizes every call.
    No source/effect/approval capability crosses this wire. *)

module Request : sig
  type t =
    | Capabilities
    | Submit of Developer_common.Submit.t
    | Observe of Developer_common.Lookup.t
    | Await_terminal of Developer_common.Lookup.t
    | Cancel_and_wait of Developer_common.Lookup.t
    | Approval_list of Id.Developer_run.t
    | Approval_respond of Developer_approval.Response.t
    | Approval_cancel of Id.Developer_run.t * Id.Permission.t
    | Watch of Id.Developer_run.t
    | Unwatch of Id.Developer_run.t

  val method_name : t -> string
  val params : t -> Jsonaf.t

  val of_method_and_params
    :  limits:Developer_common.Limits.t
    -> method_:string
    -> params:Jsonaf.t
    -> (t, Error.t) result

  val supported_methods : string list
end

module Response : sig
  type t =
    | Capabilities of Developer_common.Capabilities.t
    | Submit of Developer_common.View.t
    | Observe of Developer_common.View.t option
    | Await_terminal of Developer_common.View.t
    | Cancel_and_wait of Developer_common.View.t
    | Approval_list of Developer_approval.Page.t
    | Approval_respond
    | Approval_cancel
    | Watch
    | Unwatch

  (** Distinct constructors retain exact affinity even where several methods
      return the same View shape or an empty acknowledgement. *)
  val method_name : t -> string

  (** Encode only after validating the complete method result, including
      Observe framing, against the advertised aggregate response bound. *)
  val to_json : t -> limits:Developer_common.Limits.t -> (Jsonaf.t, Error.t) result

  val of_method_and_json
    :  limits:Developer_common.Limits.t
    -> method_:string
    -> Jsonaf.t
    -> (t, Error.t) result

  val supported_methods : string list
end

module Change : sig
  (** Pure bounded process-local invalidation notification. This codec
      installs no publisher, watch, durable cursor or live stdout/progress
      stream. After a hint or a missed connection, Observe/Approval_list
      remain authoritative. *)
  type kind =
    | Command_changed of Idempotency_key.t
    | Approval_changed

  type t = private
    { run_id : Id.Developer_run.t
    ; kind : kind
    }

  val create
    :  limits:Developer_common.Limits.t
    -> run_id:Id.Developer_run.t
    -> kind:kind
    -> (t, Error.t) result

  val to_json : t -> Jsonaf.t
  val of_json : limits:Developer_common.Limits.t -> Jsonaf.t -> (t, Error.t) result
end
