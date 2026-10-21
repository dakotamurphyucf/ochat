open Core
module P = Agent_protocol
module Bundle = P.Authoring_bundle.Bundle
module Config = Authoring_upload_access_configuration
module Contracts = Authoring_standard_contracts
module Host = Authoring_upload_host

(** Immutable startup binding of explicit administrator grants and preadmitted
    Standard selections. No Source_host, Store, actor object, filesystem path,
    callback registry or client manifest survives in this binding. *)
type t

module Error : sig
  type t =
    | Invalid_configuration
    | Missing_standard_target
    | Selection_limit
  [@@deriving sexp_of]
end

(** Revalidate configuration and resolve each selected target from the SAME
    bounded Standard cache used by configured native access. Precompute
    Rebind_policy.Selected contract identity for each distinct configured
    principal/kind/request/environment; no conversion or I/O occurs inside
    a request or a final nonyielding selection callback. *)
val bind : Config.t -> contracts:Contracts.t -> (t, Error.t) result

(** Current exact actor ID/kind and grant check. Upload_write gates begin,
    chunk, status and cancel; Commit requires Upload_write AND Publish;
    Publish alone gates the already sealed marker operation. Store still
    enforces exact principal ownership of each stage. *)
val upload_policy : t -> Host.Upload_policy.t

(** For Project basis, select by exact genuine Manifest.Project.selection. For
    ChatMD, first require one exact Script.Selector member of that bounded
    manifest, then use its Selected_project selection; Source_only supplies no
    selected Project. These are coordinates for an administrator mapping,
    never native source/path grants. An actor without Publish or a missing
    mapping returns None. A configured target has no previous Build of its
    own; a required Compose prior succeeds only when the physical Core parent
    supplies the exact Build before captured-only closure validation. *)
val publication_policy : t -> Host.Publication_policy.t

(** Independent Bundle_read AND Rebind permissions are checked before saved
    metadata lookup and at each final currentness boundary. The selector uses
    the same preadmitted target mapping but rechecks the current read actor;
    a saved receipt/digest never selects a new contract. *)
val rebind_policy : t -> Host.Rebind_policy.t
