open Core
module Attempt = Project_prepare_attempt
module Error = Project_prepare_error
module Project = Chatml_project
module Build = Chatml_project_build
module Roots = Chatml_local_roots
module Path = Source.Portable_path

module Location : sig
  type t =
    | Local of
        { cursor : Roots.Cursor.t
        ; project : Path.t
        }
    | Git of
        { repository : string
        ; pin : Project.Git_pin.t
        ; package_root : Path.t option
        ; project : Path.t
        }

  val origin : t -> Project.Lock.Catalog.origin
  val project : t -> Path.t
end

(** Invocation-owned iterative queue and capture identity table. Queue insertion,
    retained input-sized facts, comparisons and queue visits charge before work.
    Local cursors are borrowed only while their Roots owner remains alive. *)
type t

type node

type job = private
  { location : Location.t
  ; selection : Project.Selection.request
  ; namespace_override : string option
  ; previous_owner : Build.Selection_gate.Owner.t option
  ; parent : (node * Project.Dependency.t) option
  }

val create : Attempt.t -> t

val enqueue
  :  t
  -> location:Location.t
  -> selection:Project.Selection.request
  -> namespace_override:string option
  -> previous_owner:Build.Selection_gate.Owner.t option
  -> parent:(node * Project.Dependency.t) option
  -> (unit, Error.t) Result.t

(** None takes the next job. Some takes the next matching Git job,
    leaving Local and foreign repository work queued for a later nonnested bracket. *)
val take
  :  t
  -> repository:(string * Project.Git_pin.t) option
  -> (job option, Error.t) Result.t

val find : t -> Location.t -> (node option, Error.t) Result.t

(** The graph must charge one package BEFORE its acquisition. Add only stores
    the completed immutable catalog and rejects a duplicate location. *)
val add : t -> Location.t -> catalog:Chatml_catalog.t -> (node, Error.t) Result.t

val catalog : node -> Chatml_catalog.t
val location : node -> Location.t

(** Validate effective namespace and own environment against the exact captured
    declaration. Root Entry and dependency Library may share one catalog when
    their actual own environment agrees. No namespace or selector first-match. *)
val check_selection : t -> node -> job -> (unit, Error.t) Result.t

(** Register the incoming owner-local association after target capture/reuse.
    Reject inconsistent targets for one actual owner/name. *)
val connect : t -> job -> node -> (unit, Error.t) Result.t

(** Mark package/lexical-route/prior-owner triples, not just packages, so every
    converging native alias and prior route propagates through descendants. Returns true only on the first visit. *)
val visit : t -> node -> job -> (bool, Error.t) Result.t

(** Materialize completed immutable inputs. No cursor/native identity is retained;
    the acquisition label is a nonempty attempt-only node number. *)
val inputs
  :  t
  -> root:node
  -> (Build.Catalog_input.t * Build.Catalog_input.t list, Error.t) Result.t
