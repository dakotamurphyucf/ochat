open! Core
module Content = Instruction_content

(** Lower admitted-source discovery only. No activation, session mutation, tool
    registration, auxiliary-file reads or permission inferred from metadata. *)
module Root : sig
  (** Borrowed physical Source_host/root binding and explicit administrative ID.
      Host close/revocation invalidates it; it owns no new descriptor or store. *)
  type t
end

module Candidate : sig
  (** Ephemeral receipt bound to the exact Root binding, not just its label. *)
  type t

  val observed : t -> Content.Observed.t
end

module Error : sig
  type t =
    | Foreign_candidate
    | Limit_exceeded
    | Missing
    | Source of Authoring_source_host.Error.t
    | Content of Content.Error.t

  val sexp_of_t : t -> Sexp.t
end

module Issue : sig
  type t

  val member : t -> Source.Portable_path.t
  val error : t -> Error.t
end

module Discovery : sig
  type t

  val candidates : t -> Candidate.t list
  val issues : t -> Issue.t list
end

(** [id] is trusted host configuration, never inferred from a path or request.
    Checks the already-issued Root/current principal. No new source is opened.
    Administrative integrations must reject duplicate labels in their root map. *)
val bind_root
  :  Authoring_source_host.t
  -> actor:Operator_authorization.t
  -> root:Authoring_source_host.Root.t
  -> id:Content.Root_id.t
  -> (Root.t, Error.t) Result.t

(** Root-to-explicit-directory AGENTS chain. Only an exact absent leaf is
    optional. Invalid, denied or replaced governing content fails the operation.
    Ordered observations are not a filesystem-wide atomic snapshot. *)
val discover_project
  :  Root.t
  -> actor:Operator_authorization.t
  -> scope:Content.Scope.t
  -> limits:Content.Limits.t
  -> checkpoint:(unit -> unit)
  -> (Discovery.t, Error.t) Result.t

(** Bounded one-level directory listing, then exact slug/SKILL.md reads. Invalid
    individual skills produce bounded issues; revocation and aggregate/count/work
    exhaustion abort without publishing a partial discovery. Directory entries
    excluded by Source_host's portable-path policy remain excluded here. *)
val discover_skills
  :  Root.t
  -> actor:Operator_authorization.t
  -> limits:Content.Limits.t
  -> checkpoint:(unit -> unit)
  -> (Discovery.t, Error.t) Result.t

(** Fresh read of the receipt's exact member under current source grants.
    Changed bytes return Content.Stale; missing bytes never fall back. Calling
    discover again is explicit refresh and changes no existing Loaded value.
    Checkpoint/cancellation exceptions propagate with original identity. *)
val load
  :  Root.t
  -> actor:Operator_authorization.t
  -> Candidate.t
  -> limits:Content.Limits.t
  -> checkpoint:(unit -> unit)
  -> (Content.Loaded.t, Error.t) Result.t
