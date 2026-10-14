open! Core
module Bundle = Agent_protocol.Authoring_bundle.Bundle
module Store = Agent_store.Authoring_upload_store
module Actor = Operator_authorization
module Project = Chatml_authoring_bundle_project
module Document = Chatml_authoring_bundle_document

(** Pure captured-closure rebind under separately admitted current host policy.
    The caller owns the live Store read lease and original actor; neither a
    receipt nor this result grants future source-read or execution authority. *)

module Selected : sig
  type t

  (** Construct once from an already admitted immutable compiler contract.
      Contract identity traversal/emission is bounded to 64 MiB of charged
      work and 8 MiB of encoded identity; [admit] separately caps the sum of
      selected encoded identities at 8 MiB. [Policy.select] returns this
      precomputed value without re-encoding or other yielding work. *)
  val create
    :  environment:string
    -> contract:Chatml.Chatml_compiler_contract.t
    -> t Or_error.t

  val environment : t -> string
  val contract : t -> Chatml.Chatml_compiler_contract.t
end

module Policy : sig
  type t

  (** Both callbacks are bounded and non-yielding reads of already admitted
      host policy. [authorize_read] is independent of upload/write and message
      scopes. [select] chooses actual current target/environment/compiler
      contract for the exact manifest selector; it cannot fetch host sources,
      reload a catalog or mint a project from receipt metadata. *)
  val create
    :  authorize_read:(Actor.t -> bool)
    -> select:
         (Actor.t
          -> Bundle.Manifest.t
          -> Bundle.Manifest.Script.Selector.t option
          -> Selected.t option)
    -> t

  val authorize_read : t -> Actor.t -> bool
end

module Rebound : sig
  type basis =
    | Project of Project.Validated.t
    | Chatmd_document of Document.Validated.t

  type t

  val receipt : t -> Bundle.Receipt.t
  val basis : t -> basis
end

module Error : sig
  type t =
    | Selection_unavailable
    | Stale_capture
    | Invalid_closure
    | Resource_limit
end

type t

(** Freeze every current selected target before this request's captured source
    bytes are verified/materialized. The Store's trusted metadata callback calls
    this only after the original actor and independent read grant pass. It
    treats the saved receipt/manifest solely as comparison coordinates. *)
val admit
  :  Policy.t
  -> actor:Actor.t
  -> receipt:Bundle.Receipt.t
  -> manifest:Bundle.Manifest.t
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) result

(** Repeat actual current target selections after waits. The Host separately
    repeats the independent read grant first, including immediately before
    returning. Same contract/environment and exact target set are required;
    no cached result is itself current authority. *)
val check_current
  :  t
  -> policy:Policy.t
  -> actor:Actor.t
  -> checkpoint:(unit -> unit)
  -> bool

(** Restore each published final lock through Build.restore under the genuine
    selected compiler contract, reparse ChatMD and bind each actual declaration
    through the existing captured-only validator path. The Store compares the
    complete saved receipt basis and every graph byte against this genuine
    result; this module never projects an alternate receipt. Cancellation and
    unexpected checkpoint exceptions preserve identity. *)
val validate
  :  t
  -> capture:Store.Published_capture.t
  -> document_dir:Eio.Fs.dir_ty Eio.Path.t
  -> limits:Project.Limits.t
  -> checkpoint:(unit -> unit)
  -> (Rebound.t, Error.t) result
