(** Immutable, content-verified ChatMD prompt revision artifacts. *)

module Source : sig
  type t = private
    { relative_path : string
    ; contents : string
    ; sha256 : string
    }

  val create : relative_path:string -> contents:string -> (t, Store_error.t) result
end

module Artifact : sig
  type t = private
    { revision_id : Agent_protocol.Id.Prompt_revision.t
    ; prompt_definition_id : Agent_protocol.Id.Prompt_definition.t option
    ; canonical_source : string option
    ; root_relative_path : string
    ; root_chatmd : string
    ; root_sha256 : string
    ; sources : Source.t list
    ; parser_schema_version : int
    ; runtime_schema_version : int
    ; shell_manifest_sha256 : string option
    ; manifest_sha256 : string
    ; created_at : Agent_protocol.Timestamp.t
    }

  val create
    :  revision_id:Agent_protocol.Id.Prompt_revision.t
    -> ?prompt_definition_id:Agent_protocol.Id.Prompt_definition.t
    -> ?canonical_source:string
    -> ?root_relative_path:string
    -> root_chatmd:string
    -> sources:Source.t list
    -> parser_schema_version:int
    -> runtime_schema_version:int
    -> ?shell_manifest_sha256:string
    -> created_at:Agent_protocol.Timestamp.t
    -> unit
    -> (t, Store_error.t) result
end

type t

val create : env:Eio_unix.Stdenv.base -> root:string -> (t, Store_error.t) result

(** [install] writes owner-read-only files into an exclusive staging directory
    and atomically renames it to the revision ID. Directories remain owner-managed
    for pruning. File permissions are not a sandbox against the owning account. *)
val install
  :  t
  -> transaction_id:Agent_protocol.Id.Transaction.t
  -> Artifact.t
  -> (unit, Store_error.t) result

(** [load] verifies the manifest, every source digest and the materialized tree.
    Missing, altered, unexpected or symlinked tree files fail closed. *)
val load : t -> Agent_protocol.Id.Prompt_revision.t -> (Artifact.t, Store_error.t) result

(** Bounded inventory/digest verification against an independently retained
    admission digest. Uses the caller's aggregate reader budget, rejects links,
    unknown/missing files and mismatched identity, and does not compile sources.
    The caller must own and serialize the artifact root. Verification alone does
    not prove that the artifact is unreferenced or authorize deletion. *)
val verify_retained
  :  t
  -> reader:Retention_reader.t
  -> revision_id:Agent_protocol.Id.Prompt_revision.t
  -> manifest_sha256:string
  -> (unit, Store_error.t) result

(** [verify_materialized_tree t artifact] verifies the exact materialized file
    inventory and contents against [artifact], without following symbolic links.
    Cancellation propagates. This is load-time verification, not continuous
    filesystem monitoring or isolation from concurrent writes by the owner. *)
val verify_materialized_tree : t -> Artifact.t -> (unit, Store_error.t) result

(** [verify_tree ~root artifact] applies the same verification to an existing
    tree capability, including before constructing a runtime from a cached revision. *)
val verify_tree
  :  root:Eio.Fs.dir_ty Eio.Path.t
  -> Artifact.t
  -> (unit, Store_error.t) result

val exists : t -> Agent_protocol.Id.Prompt_revision.t -> bool

(** [prune_unreferenced t ~protected] removes installed revision directories
    not present in [protected]. Staging directories and malformed entries are
    never selected for deletion. *)
val prune_unreferenced
  :  t
  -> protected:Agent_protocol.Id.Prompt_revision.t list
  -> (int, Store_error.t) result

(** [materialized_tree] returns the confined Eio source tree for a verified
    installed revision. *)
val materialized_tree
  :  t
  -> Agent_protocol.Id.Prompt_revision.t
  -> Eio.Fs.dir_ty Eio.Path.t
