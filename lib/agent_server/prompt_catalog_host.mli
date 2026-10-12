open Core

(** Daemon-owned catalog preparation and dependency-history integration. Source
    authority comes only from Project_source_host; original source scopes and
    persistence remain on the actual host switch. Operations belong to the creating
    Eio domain, as enforced by the underlying source and publication owners. *)
type t

module Store_origin : sig
  (** Supplied only from the actual successful Session_store branch while its
      exclusive host lock is held. Index absence never establishes Created. *)
  type t =
    | Created
    | Existing
end

module Error : sig
  type t =
    | Store of Agent_store.Store_error.t
    | History of Agent_session.Prompt_project_baseline_store.Error.t
    | Sources of Project_source_host.Error.t
    | Publication of Prompt_catalog_publication.Error.t
    | Preparation_timeout

  val message : t -> string
end

(** Open the actual artifact store and indexes directory; create baseline history
    only for Created, otherwise preserve missing/corrupt history as Untracked.
    Does not prepare or install definitions. [sources] and [sw] must outlive this
    owner and all pending preparations. Its own switch finalizers join history
    work before releasing the metadata directory. Fixed server history ceilings:
    1024 definitions,65536 references,4096 identity bytes,8MiB document,10m work. *)
val create
  :  sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> store:Agent_store.Session_store.t
  -> origin:Store_origin.t
  -> sources:Project_source_host.t
  -> content_limits:Chatmd_prompt_content.Limits.t option
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) Result.t

val catalog : t -> Agent_session.Prompt_catalog.t

(** Pure last-installed metadata, including while publication is busy/uncertain.
    No filesystem read or source acquisition. *)
val history : t -> Prompt_catalog_publication.History.t

module Prepared : sig
  type t

  val entries : t -> Agent_session.Prompt_catalog.entry list
  val previous_history : t -> Prompt_catalog_publication.History.t
  val history : t -> Prompt_catalog_publication.History.t
end

(** One original monotonic attempt deadline spans source borrowing, all definitions
    and prewriter publication checkpoints. Individual source budgets do not renew
    it. Borrowed source handles end before return. If delivering a prepared result
    fails/cancels, discard its pending candidate before propagating the original
    exception. No extra source acquisition occurs during publish/commit/retire. *)
val prepare
  :  t
  -> mode:Prompt_catalog_publication.Mode.t
  -> created_at:Agent_protocol.Timestamp.t
  -> checkpoint:(unit -> unit)
  -> Agent_session.Prompt_definition.t list
  -> (Prepared.t, Error.t) Result.t

val publish : Prepared.t -> (unit, Error.t) Result.t

(** Same-domain, preallocated, nonyielding pointer phase after acknowledgment.
    Daemon combines it with Factory/config installation without intervening IO. *)
val commit : Prepared.t -> (unit, Prompt_catalog_publication.Error.t) Result.t

val discard : Prepared.t -> Prompt_catalog_publication.certainty
val retire : Prepared.t -> unit

val prune
  :  t
  -> additional:Agent_protocol.Id.Prompt_revision.t list
  -> checkpoint:(unit -> unit)
  -> (int, Error.t) Result.t

(** Actual Builder child callback: borrow this host's administrative source grants,
    preserve current dependency history and the original child Startup/Scope.
    Source diagnostics retain their original values; admission failures become
    finite host diagnostics. No additional authority escapes. No index publication
    or fresh artifact retention is performed by this custody-only operation. *)
val prepare_child_programs
  :  t
  -> startup:Chat_response.Startup_compilation.t
  -> invocation:Agent_session.Runtime_script_preparation.Invocation.t
  -> scope:Agent_session.Prompt_source_owner.Scope.t
  -> capture:Agent_session.Prompt_source_owner.Capture.t
  -> checkpoint:(unit -> unit)
  -> ( Chatml_prompt_programs.t
       , Agent_session.Prompt_revision_builder.Diagnostic.t list )
       Result.t
