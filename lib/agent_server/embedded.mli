open! Core

(** Process-local host using the same daemon composition, session actor, and
    client protocol path as networked sessions. *)

type options =
  { prompt_file : string
  ; workspace : string
  ; tool_dir : string
  ; home : string option
  ; storage : Local_storage.t
  ; start_immediately : bool
  ; permission_profile : Config.Permission_profile.t
  ; attachment_mode : Agent_protocol.Session.attachment_mode
  ; event_capacity : int
  }

type t

val default_permission_profile : Config.Permission_profile.t

(** Explicit process-local authorization permits the compiled shell manifests to
    load. Ordinary tool approval, shell command policy, capability checks and
    sandbox enforcement remain unchanged. With [false], startup requires a grant.
    This does not change daemon configuration or persist an operator grant. *)
val interactive_permission_profile
  :  authorize_shell_manifest:bool
  -> Config.Permission_profile.t

(** [daemon_options] supplies the shared runtime's trusted host configuration,
    including provider adapters, policy and internal extension qualification.
    Defaults match [Daemon.default_options]. The embedded host always derives
    extension host metadata from explicit [storage], keeps process-bound liveness and
    starts no network listener. A durable data root preserves data; it does not
    keep jobs running after the embedding process exits. Startup initializes the
    Unix cryptographic RNG before allocating IDs or a transient data root. *)
val start
  :  sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> ?daemon_options:Daemon.options
  -> ?authoring_package_files:string list
       (** Absolute paths captured through the shared bounded loader before store
         creation. Packages configure the same host as daemon configuration;
         they do not enable extensions or widen selected tool authority. *)
  -> ?authoring_budget:Chat_response.Authoring_validation.context_budget
       (** Query defaults/ceiling and automatic insertion budget, shared with
           daemon configuration and inherited by delegated sessions. *)
  -> options
  -> (t, Agent_protocol.Error.t) result

(** A host owner independent of session creation. Operator_only starts no
    recovered runtime/scheduler and rejects activating session operations.
    Durable local operator identity requires private OS-owned root permissions. *)
type host

val open_host
  :  sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> ?daemon_options:Daemon.options
  -> startup_mode:Daemon.startup_mode
  -> config:Config.t
  -> tool_dir:string
  -> home:string
  -> event_capacity:int
  -> unit
  -> (host, Agent_protocol.Error.t) result

(** Additions to Embedded: reuse the exact local composition and actual host
    cleanup owner without creating a replacement session. *)
val open_local_host
  :  sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> ?daemon_options:Daemon.options
  -> ?authoring_package_files:string list
  -> ?authoring_budget:Chat_response.Authoring_validation.context_budget
  -> options
  -> (host, Agent_protocol.Error.t) result

(** Caller owns open host. Select exact current anchor before attachment. Saved
    prompt/workspace/permissions are authoritative; mode only requests attachment
    access and cannot replace them.
    Stopped selection does not start. Failure leaves caller owning this host. *)
val attach_retained
  :  host
  -> mode:Agent_protocol.Session.attachment_mode
  -> expected:Agent_protocol.Session_lifecycle.Expected.t
  -> (t, Agent_protocol.Error.t) result

(** Scope one local host through a callback without transferring its owner out.
    On rejection/exception preserve the original failure and protected cleanup;
    failed cleanup fails the actual owning Switch with retained diagnostics.
    On success close this host before returning the callback value. The callback
    must not return the host, its connection, or resources owned by it. *)
val with_local_host
  :  sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> ?daemon_options:Daemon.options
  -> ?authoring_package_files:string list
  -> ?authoring_budget:Chat_response.Authoring_validation.context_budget
  -> options
  -> f:(host -> ('a, Agent_protocol.Error.t) Result.t)
  -> ('a, Agent_protocol.Error.t) Result.t

val host_connection : host -> Agent_client.Connection.t

(** Trusted local composition currently grants the same compiled local scopes
    as [start]; it is not a configurable network-principal mapping. Operator-only
    dispatch further restricts executable methods. Provider scopes belong to the
    separate operator service contract. *)
val host_principal : host -> Agent_protocol.Principal.t

val host_dispatcher : host -> Dispatcher.t
val connect_host : host -> (Agent_client.Connection.t, Agent_protocol.Error.t) result
val close_host : host -> unit
val connection : t -> Agent_client.Connection.t
val session_id : t -> Agent_protocol.Id.Session.t
val attachment : t -> Agent_protocol.Session.Attachment.t
val dispatcher : t -> Dispatcher.t
val principal : t -> Agent_protocol.Principal.t

(** Compatibility API: caller must retain an open host; closed hosts raise. *)
val connect : t -> Agent_client.Connection.t

val close_connection : t -> Connection_context.t -> unit
val close : t -> unit

(** Open a distinct connection owned by this embedded host. The caller borrows
    it until host close, may install one notification consumer, and must not
    independently close or escape it. Acquire and close on the host owning Eio
    domain. Acquisition/adoption do not yield on that domain, so closed admission
    rejects acquisition before constructing a client. Existing [connect] retains its
    caller-owned contract. All owned connection cleanup precedes daemon/store
    release and preserves original failure plus cleanup diagnostics. *)
val connect_owned : t -> (Agent_client.Connection.t, Agent_protocol.Error.t) result

(** Borrow this started session within its existing ownership Switch and close its
    actual host before returning. Callback must not escape usable session-owned
    resources or recursively close the host. Result rejection, exception and
    cancellation preserve the original primary/backtrace and secondary cleanup
    diagnostic using the concrete host owner; repeated cleanup failure fails that
    owning scope. *)
val with_session
  :  t
  -> f:(t -> ('a, Agent_protocol.Error.t) result)
  -> ('a, Agent_protocol.Error.t) result

(** Promote the exact retained owner after current local-principal authorization.
    Stopped selection does not start a session; reads/replay/gate commits never call it. *)
val select_session
  :  host
  -> Agent_protocol.Session_lifecycle.Expected.t
  -> (unit, Agent_protocol.Error.t) Result.t
