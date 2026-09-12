open! Core

(** Top-level owner of one durable agent-server instance. Construction does
    not start transport listeners; callers may bind any enabled transport to
    the returned dispatcher. *)

type status =
  | Starting
  | Ready
  | Draining
  | Stopped
  | Failed of Agent_protocol.Error.t
[@@deriving sexp]

type options =
  { implementation_name : string
  ; implementation_version : string
  ; features : string list
  ; extension_host : Agent_protocol.Extension_capabilities.host
  ; protocol_limits : Agent_protocol.Initialize.Limits.t
  ; timing : Agent_protocol.Initialize.Timing.t
  ; factory_limits : Session_factory.limits
  ; quota_limits : Agent_session.Quota_manager.limits
  ; reviewer_resolver : Catalog_builder.reviewer_resolver option
  ; policy_evaluator_resolver : Catalog_builder.policy_evaluator_resolver option
  ; model_post_stream : Agent_session.Runtime_builder.model_post_stream option
  ; qualify_chatml_extensions : bool
    (** Enable the shared extension runtime; true by default. False is an explicit
        embedding compatibility override and suppresses extension discovery.
        Individual tools still require ChatMD declarations and normal authority. *)
  ; session_helpers : Agent_session.Session_management_channel.grant list
    (** Trusted host opt-ins for named shell tools using the private helper
        channel. Empty by default. Each grant must validate its helper's actual
        filesystem/environment boundary against this host's credentials/control
        endpoints. Alternatively use [Config.Server.session_helpers]; these two
        grant sources cannot be combined. *)
  ; independent_lifetime_policy : string option
    (** Explicit trusted host policy revision authorizing independent lifetime.
        None denies it. The revision is hashed into private admissions; changing
        it invalidates earlier grants. This is not a model-supplied bearer token
        and does not enable extensions when the host override disables them. *)
  ; chatml_runtime_policy : Chat_response.Runtime_semantics.policy
    (** Initial policy for newly qualified runtimes. Existing sessions retain
        their recorded policy across reload/restart. No CLI/configuration flag. *)
  ; authoring_validation_host : Chat_response.Authoring_validation.host option
    (** Optional target identity/policy for readonly helpers. With
        extensions enabled, None selects the installed native
        targets/catalog. An explicit host retains its target/compiler/source
        restrictions. This does not grant tool execution authority. *)
  ; oauth_resolver : (string -> Authenticator.bearer_validator option) option
  }

type t

val default_options : options

val start
  :  sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> config:Config.t
  -> tool_dir:string
  -> home:string
  -> process_start_identity:string option
  -> ?options:options
  -> unit
  -> (t, Agent_protocol.Error.t) result

val status : t -> status
val dispatcher : t -> Dispatcher.t
val registry : t -> Session_registry.t
val store : t -> Agent_store.Session_store.t

(** Internal qualified host services; access is not caller authorization. *)
val factory : t -> Session_factory.t

val blob_store : t -> Agent_store.Blob_store.t
val prompts : t -> Agent_session.Prompt_catalog.t
val workspaces : t -> Agent_session.Workspace_catalog.t
val close_connection : t -> Connection_context.t -> unit

(** [reload_config] validates and atomically publishes catalog changes.
    Server/listener/storage changes are rejected as restart-required. *)
val reload_config : t -> (Config_diff.t, Config.Diagnostic.t list) result

(** Authenticates one HTTP bearer credential according to the validated
    listener configuration. [None] is accepted only in explicit development
    anonymous mode. *)
val authenticate_http_bearer
  :  t
  -> string option
  -> (Agent_protocol.Principal.t, Agent_protocol.Error.t) result

(** [authenticate_http] evaluates trusted reverse-proxy identity first, then
    configured static/OAuth bearer validators, then explicit development
    anonymous mode. *)
val authenticate_http
  :  t
  -> Authenticator.Request_identity.t
  -> string option
  -> (Agent_protocol.Principal.t, Agent_protocol.Error.t) result

(** [import_legacy t ~principal ~source_id ~source_path ~legacy request]
    imports and registers a stopped legacy session without opening a client
    transport. *)
val import_legacy
  :  t
  -> principal:Agent_protocol.Principal.t
  -> source_id:string
  -> source_path:string
  -> legacy:Session.t
  -> Agent_protocol.Session.Create_request.t
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

(** Returns the current daemon health projection without requiring an RPC
    connection. *)
val health : t -> include_details:bool -> Agent_protocol.Health.Response.t

(** Stops accepting semantic work, terminates loaded sessions, and releases
    the durable data-root lock. It is idempotent.
    Close new scheduler admission and cancel running jobs, allowing already-admitted
    timer and job-completion callbacks to finish within the configured shutdown
    grace. Grace expiry cancels runtimes normally; interrupted handler receipts
    remain non-replayable. Actor/resource cleanup always runs under cancellation
    protection and can outlive that grace. *)
val shutdown : t -> (unit, Agent_protocol.Error.t) result
