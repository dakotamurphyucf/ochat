open! Core

(** Explicit standalone qualification only; no normal/CI live alias. There is no
    provider shim, request rewriting, environment credential fallback or fake
    expiry. The runner exclusively owns one persistent session/plan budget. *)
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
    | Renew
    | Logout
    | Feature
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

    Feature plans require Feature phase, a fresh isolated root and one attempt.
    JSON-schema/image/reasoning use the actual no-tool Session/Host path. Document and function-call API probes use one actual auxiliary Execution.run
    with the same session actor, durable ledger and budget; they do not start or
    commit a session turn or execute a tool. Other authentication routes return
    finite Incomplete with zero dispatch. Document qualifies inline API file input,
    not frontend/session attachments. Existing phases reject feature
    plans; no effects are repeated on a reused root. *)
val run
  :  env:Eio_unix.Stdenv.base
  -> plan:Plan.t
  -> phase:Plan.phase
  -> root:string
  -> key_input:Key_input.t option
  -> hold_until_expiry:bool
  -> browser_presentation:Browser_presentation.t
  -> Evidence.t
