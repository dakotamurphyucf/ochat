open! Core

(** Explicit standalone qualification only; no normal/CI live alias. There is no
    provider shim, request rewriting, environment credential fallback or fake
    expiry. The runner exclusively owns one persistent root/plan budget; an
    explicit rejection probe admits its separate auxiliary session once. *)
module Key_input : sig
  type t

  val private_file : string -> (t, string) Result.t
  val environment : string -> (t, string) Result.t
end

module Browser_presentation : sig
  type t =
    | Private_terminal
    | Launch_local
  [@@deriving equal]
end

module Plan : sig
  type auth =
    | Api
    | Browser
    | Device
  [@@deriving equal]

  type transport =
    | Sse
    | Require_websocket
  [@@deriving equal]

  type phase =
    | Journey
    | Enrolled_journey
    | Renew
    | Logout
    | Feature
    | Rejection_probe
  [@@deriving equal]

  type t

  val create
    :  ?feature:Feature_case.Case.t
    -> auth:auth
    -> transport:transport
    -> model:string
    -> account_alias:string
    -> max_attempts:int
    -> maximum_phase:Time_ns.Span.t
    -> settings:Openai.Responses_driver.Setting.t list
    -> unit
    -> (t, string) Result.t
end

module Evidence : sig
  type status =
    | Incomplete
    | Live_attempted
    | Live_pass
    | Unsupported

  type t

  val to_json : t -> Jsonaf.t
  val write : t -> Eio.Fs.dir_ty Eio.Path.t -> unit
end

(** Pure deterministic checks only. Does not open a host, read credentials,
    resolve DNS, connect a socket or execute a model. *)
val self_check : unit -> unit

(** Run only after explicit CLI opt-in. [key_input] is an explicitly declared private local file or named environment input,
    opened through the protected storage reader after owner authorization;
    OAuth interaction uses a private terminal or explicit bounded local browser
    launch; browser launch never prints its private URI. Result
    artifacts contain allowlisted metadata and never challenge/token/history.
    Login diagnostics retain only existing closed OAuth stage/code; no raw errors.

    Feature plans require Feature phase, a fresh isolated root and one attempt.
    JSON-schema/image/reasoning use the actual no-tool Session/Host path. Document and function-call API probes use one actual auxiliary Execution.run
    with the same session actor, durable ledger and budget; they do not start or
    commit a session turn or execute a tool. Other authentication routes return
    finite Incomplete with zero dispatch. Document qualifies inline API file input,
    not frontend/session attachments. Existing phases reject feature
    plans; no effects are repeated on a reused root. Explicit resume_enrolled is
    restricted to a pre-session OAuth Journey with an unchanged existing plan,
    empty authoritative session store and exact owned original committed enrollment.
    It skips only initial acquisition; a distinct stable cancellation probe still
    runs, preserving the earlier failed receipt. Rejection_probe instead retains
    an original HTTP400 failed session and admits exactly one new no-tool auxiliary
    request under a separate immutable intent/checkpoint and remaining plan budget;
    each invocation requires a fresh bounded probe_id. All existing authorized
    sessions must have complete tracking, terminal or empty ledgers and no active
    work/effects; their actual rows consume the original global budget. Earlier
    sessions and admissions remain intact. Current captured configuration may
    evolve while route/account/model identity and explicit plan settings remain
    checked. It never retries an old request or qualifies a successful journey.
    Enrolled_journey admits one fresh full journey under separate immutable
    intent/session/history checkpoints, at most four new attempts plus all prior
    actual rows. It uses current host credentials without acquisition and retains
    the original rejection/probe sessions. enrolled_session selects only this
    checkpoint for subsequent Renew/Logout. resume_enrolled_journey is an explicit
    Enrolled_journey-only predispatch recovery: exact existing intent/checkpoint,
    zero known ledger rows and no active work/permissions/invocations/effect. It
    reuses that session after graceful-stop of validated prior idle sessions;
    any admitted inference makes this option unavailable. No receipt is reset.
    advance_expiry is Renew-only and exclusive with hold_until_expiry. It warms
    an actual request with unchanged credentials, advances only the test Host
    expiry clock for the next ordinary inference, and requires real OAuth
    validation to restore that clock before renewed grant admission. Network and
    monotonic clocks remain real; finite evidence labels controlled_host_expiry,
    never natural-expiry proof. Cleanup restores the override on all exits. *)
val run
  :  env:Eio_unix.Stdenv.base
  -> plan:Plan.t
  -> phase:Plan.phase
  -> root:string
  -> key_input:Key_input.t option
  -> hold_until_expiry:bool
  -> advance_expiry:bool
  -> browser_presentation:Browser_presentation.t
  -> resume_enrolled:bool
  -> probe_id:string option
  -> enrolled_session:bool
  -> resume_enrolled_journey:bool
  -> Evidence.t
