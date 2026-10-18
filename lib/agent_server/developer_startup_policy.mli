open! Core

(** Startup-only composition of an explicitly configured Developer service.
    This module creates no source owner, Standard cache, Store, scheduler or
    provider client. Daemon owns and joins all physical resources supplied
    here. Existing embedding-supplied Developer configurations are independent
    and must not be combined with this declarative path. *)

(** Bind a validated declarative profile to the already-open physical source
    host, Authoring installation and SAME shared Standard cache. The optional
    configured-project binding is issued by [source_host] and comes from the
    exact [read_access] installed into the Authoring owner; absent binding
    rejects Local_* and Host_project routes instead of selecting by path.
    The original read/upload config is used only for pure startup consistency
    checks, never to reopen a root or mint authority. Uploaded and document
    routes require their corresponding installed Authoring owner/policy. All
    route callbacks compare the actual authenticated
    actor, source context, script declaration and environment; they create no
    opaque Root from a configured name. Selected capture and native dispatch
    still enforce their ordinary physical source/Root/currentness checks.

    The one [inference_policy] is also passed to Session_factory. Only
    Provider_default invokes its same-host default capture callback and
    resolves that exact target before installation; capture/resolve failure
    rejects startup. Disabled never captures a target. Construct bounded
    Developer Host, optional Once approval/REPL and portable policies through
    their existing validated constructors. The selected-Prompt native mode
    retains actual Permission_policy, while Shell still requires its exact
    Manifest_grant and Once decision. No permissive callback is installed.
    Callback cancellation and unexpected exceptions propagate unchanged. *)
val bind
  :  Developer_startup_configuration.t
  -> source_host:Project_source_host.t
  -> authoring:Authoring_installation.t
  -> source_configuration:Project_source_configuration.t
  -> read_access:Authoring_project_access_configuration.t option
  -> upload_access:Authoring_upload_access_configuration.t option
  -> configured_project:Authoring_configured_project_policy.t option
  -> contracts:Authoring_standard_contracts.t
  -> inference_policy:Session_factory.inference_policy
  -> initialize_limits:Agent_protocol.Initialize.Limits.t
  -> (Developer_daemon_configuration.t, Agent_protocol.Error.t) result
