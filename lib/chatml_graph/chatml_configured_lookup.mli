open Core
module Captured = Chatml_source
module Prepared = Chatml_prepared_program

(** Compiler-only configured lookup over an already admitted immutable inventory.
    Caller first exhausts the existing lexical lookup; this module neither scans
    syntax nor adds configured names to ordinary lexical environments. *)

module Reason : sig
  type t =
    | Value_reference
    | Open
    | Qualified_type
  [@@deriving compare, equal, sexp_of]
end

module Phase : sig
  type t =
    | Implementation
    | Interface
  [@@deriving compare, equal, sexp_of]
end

module Origin : sig
  (** Immutable provenance of a configured root or an imported member binding.
      Retains canonical target, exact relative member path, input/scope affinity
      and original configuration site, never an inference/runtime/callback owner.
      Used in persistent lexical bindings so rebinding/closure declaration order
      cannot retarget an earlier checked use. *)
  type t

  val dependency : t -> Prepared.Module_id.t
  val member_path : t -> string list
  val binding_site : t -> Chatml_diagnostic.Site.t

  (** Append a field name selected by the existing compiler syntax/lookup path;
      never generalize/copy a scheme. The checker must validate that the public
      member exists before successful graph publication. Type lookup can request
      a completed namespace before that final member validation. *)
  val member : t -> name:string -> t
end

(** One file's immutable selected scope and captured revision, associated with its
    canonical implementation node. Holds this source and scope metadata, not the
    complete inventory, checker, callback, or runtime authority. Retains the exact
    admitted Prepared fingerprint as input affinity, including selected scopes
    and configuration provenance, not just this source ID/content digest. *)
type t

(** Association lookup only, not another Prepared admission: source must be this
    node's implementation/companion, or its entry when no definition is present.
    An entry also in the catalog has the same node and ordinary companion.
    Checkpoints escape unchanged. *)
val create
  :  Prepared.t
  -> owner:Prepared.Module_id.t
  -> source:Captured.Id.t
  -> checkpoint:(unit -> unit)
  -> (t, Chatml_diagnostic.t) Result.t

val owner : t -> Prepared.Module_id.t
val source : t -> Captured.t
val phase : t -> Phase.t
val input_fingerprint : t -> Captured.Digest.t

module Reference : sig
  (** An actual configured resolution event. Reference/configuration sites retain
      only needed immutable captures. Qualified type references in a companion
      have Phase.Interface, not an assumed runtime initialization requirement. *)
  type t

  val owner : t -> Prepared.Module_id.t
  val dependency : t -> Prepared.Module_id.t
  val name : t -> string
  val reason : t -> Reason.t
  val phase : t -> Phase.t
  val input_fingerprint : t -> Captured.Digest.t
  val site : t -> Chatml_diagnostic.Site.t
  val binding_site : t -> Chatml_diagnostic.Site.t
  val member_path : t -> string list
  val origin : t -> Origin.t
end

(** Called on a genuine lexical miss at its original byte span. Consult this
    exact file's selected scope. Missing/out-of-scope failures carry the use site
    and available selected-scope/configuration site, not guessed token offsets.
    Resolve aliases to the admitted Module_id; do not load/check any module.
    Validate sites only for this real lookup. Reference copies this context's
    input affinity; another input with identical source bytes but different
    bindings cannot supply an event to the current graph. Retain no callback. *)
val resolve
  :  t
  -> name:string
  -> reason:Reason.t
  -> span:Source.span
  -> checkpoint:(unit -> unit)
  -> (Reference.t, Chatml_diagnostic.t) Result.t

(** Actual use of a lexical binding imported by configured open (or its imported
    nested module). Preserve root/configuration provenance and append the owned
    use site/reason; enforce the origin's same input/owner/source/phase affinity.
    Value_reference is a runtime dependency even inside an uncalled function.
    Open/Qualified_type are compiler dependencies only. No initialized instance
    is required to introduce or inspect these bindings. *)
val at_use
  :  t
  -> Origin.t
  -> reason:Reason.t
  -> span:Source.span
  -> checkpoint:(unit -> unit)
  -> (Reference.t, Chatml_diagnostic.t) Result.t
