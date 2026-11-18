(** One immutable shared recurrence command. The selected source/target and all
    stale-write fences enter the intent digest. The live writer attachment is
    transport context and may change when replaying the original command. *)

module Features : sig
  val control : string
end

module Source_basis : sig
  type t =
    { source : Run_source.t
    ; expected_lifecycle_revision : Session_lifecycle.Revision.t
    }
end

module Goal_owner : sig
  type t = private
    { goal_id : Id.Goal.t
    ; expected_goal_revision : int64
    }

  val create : goal_id:Id.Goal.t -> expected_goal_revision:int64 -> (t, Error.t) result
end

module Capture : sig
  (** Selector data only. The Factory captures the actual current source and
      pins on behalf of the original principal. Existing input is capped at
      256 KiB including attachments. The authored selector is a declared tool
      name, not a path, digest, native admission or artifact. *)
  type selector =
    | Existing of
        { session : Session_ref.t
        ; input : Session.Message_content.t
        }
    | Authored_child of
        { name : string
        ; mode : Run.Mode.t
        }

  type t = private
    { schedule : Recurrence_schedule.t
    ; policy : Recurrence_policy.t
    ; selector : selector
    ; goal_owner : Goal_owner.t option
    }

  val create
    :  schedule:Recurrence_schedule.t
    -> policy:Recurrence_policy.t
    -> selector:selector
    -> ?goal_owner:Goal_owner.t
    -> unit
    -> (t, Error.t) result
end

module Verb : sig
  type t =
    | Create
    | Edit
    | Enable
    | Pause
    | Retire
  [@@deriving equal, compare, sexp_of]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Action : sig
  type t =
    | Create of
        { id : Id.Recurrence.t
        ; basis : Source_basis.t
        ; capture : Capture.t
        }
    | Edit of
        { id : Id.Recurrence.t
        ; expected_control_revision : int64
        ; basis : Source_basis.t
        ; capture : Capture.t
        }
    | Enable of
        { id : Id.Recurrence.t
        ; expected_control_revision : int64
        ; basis : Source_basis.t
        }
    | Pause of
        { id : Id.Recurrence.t
        ; expected_control_revision : int64
        }
    | Retire of
        { id : Id.Recurrence.t
        ; expected_control_revision : int64
        }

  val id : t -> Id.Recurrence.t
  val verb : t -> Verb.t
  val expected_control_revision : t -> int64 option
end

module Intent : sig
  type t = private
    { generation : int
    ; expected_catalog_revision : int64
    ; key : Idempotency_key.t
    ; action : Action.t
    }

  val create
    :  generation:int
    -> expected_catalog_revision:int64
    -> key:Idempotency_key.t
    -> Action.t
    -> (t, Error.t) result

  (** Domain-separated canonical identity of the complete original public
      request including controller, key and selector. Attachment and captured
      private storage data are deliberately independent. *)
  val digest : t -> session:Session_ref.t -> (string, Error.t) result
end

module Request : sig
  type t = private
    { session : Session_ref.t
    ; attachment_id : Id.Attachment.t
    ; intent : Intent.t
    }

  val create
    :  session:Session_ref.t
    -> attachment_id:Id.Attachment.t
    -> Intent.t
    -> (t, Error.t) result

  val methods : string list
  val method_name : t -> string
  val params : t -> Jsonaf.t
  val of_method_and_params : method_:string -> params:Jsonaf.t -> (t, Error.t) result
end

module Receipt : sig
  (** Controller command commitment only. This receipt does not describe an
      occurrence admission or Run outcome and supplies no continuing authority. *)
  type t = private
    { session : Session_ref.t
    ; generation : int
    ; principal_id : Id.Principal.t
    ; key : Idempotency_key.t
    ; intent_sha256 : string
    ; recurrence_id : Id.Recurrence.t
    ; verb : Verb.t
    ; definition_revision : int64
    ; control_revision : int64
    ; catalog_revision : int64
    ; accepted_at : Timestamp.t
    }

  val create
    :  session:Session_ref.t
    -> generation:int
    -> principal_id:Id.Principal.t
    -> key:Idempotency_key.t
    -> intent_sha256:string
    -> recurrence_id:Id.Recurrence.t
    -> verb:Verb.t
    -> definition_revision:int64
    -> control_revision:int64
    -> catalog_revision:int64
    -> accepted_at:Timestamp.t
    -> (t, Error.t) result

  (** Verifies canonical original request identity and the revisions the
      owning command can commit. No-op control changes may retain their old
      control revision while still recording a new catalog receipt. *)
  val check_intent
    :  t
    -> session:Session_ref.t
    -> principal_id:Id.Principal.t
    -> intent:Intent.t
    -> (unit, Error.t) result
end

module Result : sig
  type t = Receipt.t

  val method_name : t -> string
  val payload : t -> Jsonaf.t
  val of_method_and_result : method_:string -> result:Jsonaf.t -> (t, Error.t) result
end
