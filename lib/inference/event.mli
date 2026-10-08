open! Core

(** Private provider-neutral inference evidence. No canonical allocator, host
    commit, tool executor, durable workflow completion or public projection. *)
module Terminal : sig
  type delivery =
    | Definitely_not_submitted
    | Possibly_submitted
    | Response_started
  [@@deriving equal, sexp_of]

  type incomplete_reason =
    | Output_limit
    | Filtered
    | Other
    | Unavailable
  [@@deriving equal, sexp_of]

  type auth_failure =
    | Missing
    | Denied
    | Profile_changed
    | Reauthorization_required
    | Invalid_credential
    | Timed_out
  [@@deriving equal, sexp_of]

  type transport_failure =
    | Connection
    | Timeout
    | Invalid_http
    | Invalid_content_type
    | Body_limit
    | Framing_limit
    | Protocol
    | Unsupported_transport
    | Session_closed
    | Session_busy
    | Http_status of int
  [@@deriving equal, sexp_of]

  module Provider_failure : sig
    type t =
      | Invalid_request
      | Denied
      | Rate_limited
      | Unavailable
      | Unknown
    [@@deriving equal, sexp_of]
  end

  type failure =
    | Authentication of auth_failure
    | Transport of transport_failure
    | Provider of Provider_failure.t
  [@@deriving equal, sexp_of]

  type outcome =
    | Completed
    | Refused
    | Incomplete of incomplete_reason
    | Failed of failure
  [@@deriving equal, sexp_of]

  type t

  (** Closed reason/category values contain no provider code/message/body/endpoint
      or exception text. Adapter raw detail stays in its private capture. HTTP
      status is 100..599. Authentication requires Definitely_not_submitted;
      completed/refused/incomplete/provider outcomes require Response_started.
      Transport failures preserve actual delivery uncertainty. Cancellation and
      unexpected exceptions propagate instead of manufacturing a terminal. *)
  val create
    :  scope:Transcript.Scope.t
    -> delivery:delivery
    -> outcome:outcome
    -> (t, string) Result.t

  val scope : t -> Transcript.Scope.t
  val delivery : t -> delivery
  val outcome : t -> outcome
  val equal : t -> t -> bool
  val sexp_of_t : t -> Sexp.t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> limits:Document_schema.Limits.t -> (t, string) Result.t
end

type local_execution =
  | Not_eligible
  | Tool_candidate
[@@deriving equal, sexp_of]

type view =
  | Live of Transcript.Stream.t
  | Candidate_ready of
      { item : Transcript.Item.t
      ; payload : History_entry.Payload.t
      ; local_execution : local_execution
      }
  | Terminal of Terminal.t

type t

(** Revalidates complete JSON/native bounds. Live excludes Item_finalized and
    Source_finished: only the host can publish committed entries or finish local
    output admission. Candidate requires a known semantic header matching payload
    and matching call name where applicable. Its optional entry ID is actual
    reservation evidence only. No candidate allocates a host ID or executes a call;
    duplicate/reordered keys are reconciled by the existing host turn owner.
    Tool_candidate requires Semantic.Call, namespace Absent, async Absent/false,
    and status Absent/completed. The adapter separately attests opaque caller
    semantics from its actual wire capture: a pure guard cannot inspect or prove
    provider-specific caller eligibility. This is evidence, NEVER an execution
    grant. Not_eligible retains all items, including unsupported calls and unknown
    raw metadata. Same-key conflicting eligibility must reject at the host owner;
    only Tool_candidate may reach existing registry/moderation/admission.
    Terminal ends inference only; pending jobs/scripts still own workflow life.
    There is no fabricated canonical entry to represent provider completion. *)
val create : view -> limits:Document_schema.Limits.t -> (t, string) Result.t

val view : t -> view
val scope : t -> Transcript.Scope.t
val encoded_bytes : t -> int
