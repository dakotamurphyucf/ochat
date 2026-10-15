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

module Parent_previous : sig
  type t

  (** Traverse the bounded manifest target inventory once, before selecting a
      policy target, and retain an ordered exact selector-to-prior map. The
      private caller's [select] is the live Core parent scope: it reports a
      typed missing/ambiguous required prior rather than an invented None.
      Source_only declarations have no selected Project and no prior entry.
      No source read or parser work occurs here. *)
  val prepare
    :  Bundle.Manifest.t
    -> checkpoint:(unit -> unit)
    -> select:
         (Bundle.Manifest.Script.Selector.t option
          -> checkpoint:(unit -> unit)
          -> (Chatml_project_build.t option, 'error) result)
    -> (t, 'error) result
end

(** Freeze every actual selected target before validation. Source_only scripts
    intentionally have no selected Project and cannot acquire a contract.
    [parent_previous] is [None] for a parentless upload, preserving the current
    host policy's selected previous Build. For a parent-bearing upload, the
    private Host supplies a finite exact map prepared from a live physical Core
    parent scope for every selected manifest target. Its authentic Build
    replaces an absent policy previous; a conflicting policy previous rejects
    admission rather than silently changing the selected target. A required
    Compose prior with no authentic Build is unavailable before durable
    closure rejection. The frozen raw policy selection is retained separately
    from the effective prior so [check_current] can reselect and compare the
    actual host choice after waits and at final marker admission. *)
val admit
  :  Policy.t
  -> actor:Actor.t
  -> manifest:Bundle.Manifest.t
  -> parent_previous:Parent_previous.t option
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) result

(** Re-select the same raw host targets and compare resolved environment,
    contract fingerprint, prior Prepared fingerprint and exact prior lock
    source. The parent scope independently rechecks its physical Current and
    immutable effective prior at guarded checkpoints; raw policy None never
    substitutes for missing required parent evidence. *)
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
