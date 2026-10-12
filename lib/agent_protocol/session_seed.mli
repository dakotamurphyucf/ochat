open! Core
module Error = Protocol_error

(** New stopped conversation with an already-converted neutral transcript.
    This command conveys no runtime, source, workspace or tool authority. *)
module Transcript : sig
  type t [@@deriving sexp]

  (** Validate complete neutral payloads, distinct IDs and ordered relations.
      Entries must be nonredacted Canonical entries at content revision zero;
      their role/kind must agree with the neutral payload owner. Provider item IDs,
      opaque call IDs, field order and numeric spellings remain unchanged. Missing
      archived calls remain legal under the existing relation validator.

      Bound input before derived allocations. Positive [max_entries] and
      [max_work_steps] cover this invocation, including semantic validation;
      [limits] bounds compact JSON bytes, depth, fields and nodes. Checkpoint
      exceptions propagate unchanged. This operation invokes no Converter.
      The actual new session namespace is checked separately by its state owner. *)
  val create
    :  History.entry list
    -> next_history_sequence:int64
    -> limits:Document_schema.Limits.t
    -> max_entries:int
    -> max_work_steps:int
    -> checkpoint:(unit -> unit)
    -> (t, Error.t) Result.t

  val entries : t -> History.entry list
  val next_history_sequence : t -> int64
  val to_json : t -> Jsonaf.t

  (** Strict admission under the existing 16 MiB transport/durable document
      ceiling. The sexp reader applies the same invariant checks. No client
      provenance becomes a private invocation, authorization or operation receipt. *)
  val of_json : Jsonaf.t -> (t, Error.t) Result.t
end

module Code_selection : sig
  type t =
    | Catalog of
        { definition_id : Id.Prompt_definition.t
        ; expected_revision : Id.Prompt_revision.t
        }
    | Retained of Session_lifecycle.Expected.t
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) Result.t
end

module Request : sig
  type t [@@deriving sexp]

  (** Validate an authored Catalog prompt selector and Configured workspace,
      [start_immediately=false] and ordinary Spec metadata/host combinations.
      Catalog selection must agree with [spec.prompt]; Retained selection is
      checked against the actual retained definition by the host. No initial
      attachment or subscription exists in this request.

      The original command binds the complete seed, selector, Spec and key;
      existing request and receipt ceilings still apply. There is no hidden
      upload table or truncated history. A different key is a new conversation. *)
  val create
    :  Session.Spec.t
    -> code:Code_selection.t
    -> transcript:Transcript.t
    -> idempotency_key:Idempotency_key.t
    -> (t, Error.t) Result.t

  val spec : t -> Session.Spec.t
  val code : t -> Code_selection.t
  val transcript : t -> Transcript.t
  val idempotency_key : t -> Idempotency_key.t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) Result.t
end

module Result : sig
  type t = { session_id : Id.Session.t } [@@deriving sexp]

  (** Persisted stopped identity. Activation/attachment is a subsequent separately
      authorized command. The ordinary receipt summary is Created_session. *)
  val to_json : t -> Jsonaf.t

  val of_json : Jsonaf.t -> (t, Error.t) Core.Result.t
end
