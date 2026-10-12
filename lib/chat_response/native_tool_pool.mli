open Core

(** Actual native implementations and their lazily constructed capability
    identities. Construction borrows the caller's switch and host resources; it
    does not initialize script hooks, compile managed handlers or publish tools.
    MCP declarations retain their existing discovery and switch ownership. *)

module Source_requirement : sig
  type t =
    | Ordinary
    | Source of Native_source_registration.t option
    (** [Source None] rejects queued admission without a source fallback. *)
end

module Registration : sig
  type t =
    { implementation : Ochat_function.t
    ; implementation_revision : string
    ; result_contract : Tool_capability.result_contract
    ; authoring_metadata : Chatmd_shell_spec.Authoring_metadata.t option
    ; source_requirement : Source_requirement.t
      (** Actual emitted function's source ownership. The installed-artifact
          preparer belongs to that exact function and its native/source lifetime. *)
    }
end

module Error : sig
  type t =
    { code : string
    ; message : string
    }
  [@@deriving sexp, compare, equal]
end

module Shell_view : sig
  type t =
    | Absent
    | Live of Shell_runtime.Registry.t
    | Staged of Shell_runtime.Registry.Staged.t
end

type t

(** Reject duplicate supplied names before constructing any native resources.
    Unselected registrations remain unavailable to declarations. *)
val validate_registrations : Registration.t list -> (unit, Error.t list) Result.t

(** Construct each declaration once using the existing tool dispatcher. Custom
    and shell declarations use the actual admitted registry's shell builders;
    staged shell functions retain its activation/lifetime gate. Extension tools
    are left for their managed owner and never receive a dummy native runner.
    Unexpected construction failures keep the existing diagnostic mapping;
    cancellation and timeout exceptions propagate unchanged.

    Capability metadata and secure identities remain lazy, including resource
    fingerprinting and supplied-service policy. Forcing does not reconstruct
    functions, reconnect MCP clients or invoke tools. The owner must admit the
    resulting pool and install its managed dispatch before exposing it. *)
val create
  :  sw:Eio.Switch.t
  -> ctx:Eio_unix.Stdenv.base Ctx.t
  -> host:Shell_runtime.Host.t
  -> shell:Shell_view.t
  -> admin_policy:Shell_runtime.Admin_policy.t
  -> ?native_service_revision:string
  -> ?agent_source_runner:Tool.Agent_source_runner.t
  -> registrations:Registration.t list
  -> run_agent:
       (?prompt_dir:Eio.Fs.dir_ty Eio.Path.t
        -> ?session_id:string
        -> ?observer:Agent_response_loop.observer
        -> source:string
        -> ctx:Eio_unix.Stdenv.base Ctx.t
        -> string
        -> Prompt.Chat_markdown.content_item list
        -> string)
  -> Prompt.Chat_markdown.top_level_elements list
  -> (t, Error.t list) Result.t

val functions : t -> Ochat_function.t list
val capabilities : t -> (Tool_capability.t, Tool_capability.error) Result.t Lazy.t
val classifications : t -> (string * Tool_execution_event.agent_page_kind) list
val registrations : t -> Registration.t list
