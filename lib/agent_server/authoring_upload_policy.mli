open Core
module P = Agent_protocol
module Host_policy = Authoring_installation.Host_policy

(** Startup-only composition of explicitly configured upload authority into
    the existing one Service/UploadHost/Store. No new parser, compiler,
    Publisher, daemon registry or upload file namespace is introduced. *)
type t

module Limits : sig
  type t

  (** Validated existing Store and captured-only Project limits. These caps
      remain independent of Standard contract conversion and advertised
      Initialize request size; the latter is checked against the actual
      installed Host_policy before any child owner opens. *)
  val create
    :  store:Agent_store.Authoring_upload_store.Limits.t
    -> validation:Chatml_authoring_bundle_project.Limits.t
    -> (t, P.Error.t) result

  (** Finite daemon/CLI profile based on the actual protocol member/chunk
      ceilings and same lower Store/validation constructors used in tests.
      The concrete member, bundle, reservation and retention caps are
      documented with source examples after measured fixture admission. *)
  val standard : (t, P.Error.t) result
end

(** None config leaves [base] identical, including an intentional disabled
    service. [Upload_only] explicitly discards core/native/supplied families
    and conflicts with a simultaneous native configuration. [Augment_core]
    requires a real existing core base and preserves its configured native,
    supplied, semantic and assistance policies while adding ONE upload owner.
    A base already owning upload is a typed startup conflict. The same
    immutable Standard cache serves both configured native and upload
    mappings; the configured upload mapping has no native-root grant. The
    caller invokes Authoring_installation.create exactly once after this
    pure composition. Failed startup uses its existing reverse joined cleanup;
    server config reload is restart-required. *)
val create
  :  base:Host_policy.t option
  -> configured:Authoring_upload_access_configuration.t option
  -> native_config:Authoring_project_access_configuration.t option
  -> contracts:Authoring_standard_contracts.t
  -> initialize_limits:P.Initialize.Limits.t
  -> document_dir:Eio.Fs.dir_ty Eio.Path.t
  -> limits:Limits.t
  -> (t, P.Error.t) result

val host_policy : t -> Host_policy.t option
