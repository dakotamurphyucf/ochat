open! Core

(** Neutral input values and host services. No wire codec, executable original,
    Actor, Runtime_owner, or future Operation. Constructing DATA or callbacks does
    not grant authority; actual Actor input scope owns admission and lifetime. *)
module Prepared_input : sig
  type t

  (** Public validated DATA constructor. Requires User_submission, validates a
      canonical User/Message entry, exact reserved ID, and original request
      digest/content association. It does not prove parsing or execution. *)
  val create
    :  request:Run_admission_request.t
    -> id:History_entry.Id.t
    -> entry:Agent_protocol.History.entry
    -> (t, Agent_protocol.Error.t) result

  val validate
    :  t
    -> request:Run_admission_request.t
    -> id:History_entry.Id.t
    -> (unit, Agent_protocol.Error.t) result

  val equal : t -> t -> bool
  val request_sha256 : t -> string
  val content : t -> Agent_protocol.Session.Message_content.t
  val entry : t -> Agent_protocol.History.entry
end

module Execution : sig
  (** Supplied by actual Actor Parsing scope. Every callback rejects escaped,
      Parsed_ready, Consumed, Closed, foreign or displaced scopes; fresh effects
      retain original policy/source/Run/controller checks. *)
  type t =
    { request : Run_admission_request.t
    ; id : History_entry.Id.t
    ; checkpoint : unit -> (unit, Agent_protocol.Error.t) result
      (** Store refresh outside Actor/mutex, then actual final effect checks. *)
    ; refresh :
        unit
        -> (unit -> (unit, Agent_protocol.Error.t) result, Agent_protocol.Error.t) result
      (** Refresh for existing Shell Call_context.with_narrowed_check and
          Submission_guard. Returned nonyielding final check performs selected
          current checks after inherited parent checks, ends in pure bases;
          it performs no Store/RPC. No generic close-to-Ok behavior. *)
    ; allocate_inference : Inference_allocation.Port.t
    }
end

module Tracking : sig
  (** One actual lexical input Graph_tracking, bound to Execution's allocator.
      All callbacks retain same actual input scope/preparation for ACKs. These
      replace the ordinary runtime tracker together; identity alone is unsound. *)
  type t =
    { identity : Inference_client.Identity.t
    ; on_attempt : Inference_runtime.Attempt.t -> unit
    ; on_observation : Inference.Observation.t -> unit
    ; on_completion : Inference_client.Completion.t -> unit
    }
end
