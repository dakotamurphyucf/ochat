module Bundle = Agent_protocol.Authoring_bundle.Bundle
module Store = Agent_store.Authoring_upload_store
module Actor = Operator_authorization
module Project = Chatml_authoring_bundle_project

module Selected : sig
  type t

  (** A host-admitted selection. The environment is a resolved name, not a
      client-supplied default or signature. [previous] is an actually admitted
      Build and supplies exact prior Prepared and lock identity. *)
  val create
    :  environment:string
    -> contract:Chatml.Chatml_compiler_contract.t
    -> previous:Chatml_project_build.t option
    -> t Core.Or_error.t

  val environment : t -> string
  val contract : t -> Chatml.Chatml_compiler_contract.t
  val previous : t -> Chatml_project_build.t option
end

module Policy : sig
  type t

  (** A trusted, bounded, non-yielding current host choice. None withholds a
      selected target. The callback cannot reload a catalog or acquire source
      bytes; the Store publication marker calls it at final admission. *)
  val create
    :  select:
         (Actor.t
          -> Bundle.Manifest.t
          -> Bundle.Manifest.Script.Selector.t option
          -> Selected.t option)
    -> t
end

module Error : sig
  type t =
    | Unavailable
    | Invalid_closure
    | Resource_limit
end

type t

(** Freeze every actual selected target before validation. Source_only scripts
    intentionally have no selected Project and cannot acquire a contract. *)
val admit
  :  Policy.t
  -> actor:Actor.t
  -> manifest:Bundle.Manifest.t
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) result

(** Re-select the same targets and compare resolved environment, contract
    fingerprint, prior Prepared fingerprint, and exact prior lock source.
    The host calls this after waits and on every guarded Store checkpoint. *)
val check_current
  :  t
  -> policy:Policy.t
  -> actor:Actor.t
  -> checkpoint:(unit -> unit)
  -> bool

(** Validate only the authenticated Store snapshot with the captured-only
    Project/ChatMD validators and one shared lower policy. Never read a host
    source, initialize the compiler, execute, or mint a wire receipt. *)
val validate
  :  t
  -> snapshot:Store.Snapshot.t
  -> document_dir:Eio.Fs.dir_ty Eio.Path.t
  -> limits:Project.Limits.t
  -> checkpoint:(unit -> unit)
  -> (Store.Proof.t, Error.t) result
