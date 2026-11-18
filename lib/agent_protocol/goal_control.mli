(** Shared execution and metadata requests. Transport attachment lives only in
    [Request]; replace it after a reconnect while keeping the original
    immutable intent, key and revisions for retained replay. *)
module Features : sig
  val control : string
end

module Start : sig
  (** Immutable intent for a new Goal episode. The actual owner compares this
      source and the session, Goal and catalog revisions under its writer fence. *)
  type t = private
    { generation : int
    ; expected_revision : int64
    ; source : Run_source.t
    ; goal_id : Id.Goal.t
    ; expected_goal_revision : int64
    ; expected_catalog_revision : int64
    ; input : Run_input.t
    ; key : Idempotency_key.t
    }

  val create
    :  generation:int
    -> expected_revision:int64
    -> source:Run_source.t
    -> goal_id:Id.Goal.t
    -> expected_goal_revision:int64
    -> expected_catalog_revision:int64
    -> input:Run_input.t
    -> key:Idempotency_key.t
    -> (t, Error.t) result

  val run_request_sha256 : t -> session_id:Id.Session.t -> (string, Error.t) result

  (** Exact durable Goal association identity, including the Goal and catalog
      fences. Internal starts created before source fencing use [None]. *)
  val association_sha256
    :  goal_id:Id.Goal.t
    -> expected_goal_revision:int64
    -> expected_catalog_revision:int64
    -> expected_source:Run_source.t option
    -> string
end

module Request : sig
  type 'intent target = private
    { session : Session_ref.t
    ; attachment_id : Id.Attachment.t
    ; intent : 'intent
    }

  type t =
    | Command of Goal_command.t target
    | Start of Start.t target
    | Resume of Goal_resume_request.t target

  val command
    :  session:Session_ref.t
    -> attachment_id:Id.Attachment.t
    -> Goal_command.t
    -> (t, Error.t) result

  val start
    :  session:Session_ref.t
    -> attachment_id:Id.Attachment.t
    -> Start.t
    -> (t, Error.t) result

  val resume
    :  session:Session_ref.t
    -> attachment_id:Id.Attachment.t
    -> Goal_resume_request.t
    -> (t, Error.t) result

  val session : t -> Session_ref.t
  val attachment_id : t -> Id.Attachment.t
  val methods : string list
  val method_name : t -> string
  val params : t -> Jsonaf.t
  val of_method_and_params : method_:string -> params:Jsonaf.t -> (t, Error.t) result
end

module Callback_control : sig
  type disposition =
    | Held of Goal.Reason.t
    | Abandoned of Goal.Reason.t

  (** Factual joined callback ACK, with its Goal revisions and waiting state. *)
  type t =
    { goal_id : Id.Goal.t
    ; goal_revision : int64
    ; catalog_revision : int64
    ; disposition : disposition
    ; waiting : bool
    }
end

module Start_result : sig
  module Admission : sig
    (** Both digests identify the exact immutably committed Start. The Run
        receipt binds input and session fences. The association binds the Goal
        and catalog fences and source. Neither grants execution authority. *)
    type t = private
      { receipt : Run_receipt.t
      ; association_sha256 : string
      }

    val create : Run_receipt.t -> association_sha256:string -> (t, Error.t) result
  end

  (** [Activated] describes real handoff. Retained replay supplies historical
      admission. Known admission survives subsequent failure. *)
  type t =
    | Activated of Admission.t
    | Controlled of
        { admission : Admission.t
        ; control : Callback_control.t
        }
    | Settled of
        { admission : Admission.t
        ; run_revision : int64
        }
    | Retained of Admission.t
    | Admission_uncertain of Error.t
    | Activation_failed of
        { admission : Admission.t
        ; error : Error.t
        }
end

module Resume_result : sig
  type continuation =
    | Started of
        { operation_id : Id.Operation.t
        ; generation : int
        }
    | Waiting
    | Settled of
        { run_id : Id.Run.t
        ; run_revision : int64
        }
    | Controlled of Callback_control.t

  (** Acknowledgement binds the original command to the same Run. A retained
      receipt does not restore runtime custody. [Rejected] precedes a writer
      ACK and [Uncertain] leaves the first ACK unknown. *)
  type t =
    | Resumed of
        { receipt : Goal_command_receipt.t
        ; continuation : continuation
        }
    | Retained of Goal_command_receipt.t
    | Resume_failed of
        { receipt : Goal_command_receipt.t
        ; error : Error.t
        }
    | Rejected of Error.t
    | Uncertain of Error.t
end

module Result : sig
  type t =
    | Command of Goal_command_receipt.t
    | Start of Start_result.t
    | Resume of Resume_result.t

  val method_name : t -> string
  val payload : t -> Jsonaf.t
  val of_method_and_result : method_:string -> result:Jsonaf.t -> (t, Error.t) result
end
