open Core

(** One immutable, bounded cache of actual Standard contracts for the union of
    configured native and upload target requests. The cache owns no source,
    actor, Store, parser or mutable registry. Equal target occurrences share
    the SAME admitted contract value and one conversion debit. *)
type t

module Entry : sig
  type t

  (** Comparison data from the one bounded conversion for this actual Standard
      target. It grants neither a target choice nor a native/upload operation. *)
  val contract : t -> Chatml.Chatml_compiler_contract.t

  val identity_bytes : t -> int
  val fingerprint : t -> Chatml_source.Digest.t
end

module Error : sig
  type t =
    | Invalid_limits
    | Contract_limit
  [@@deriving sexp_of]
end

(** Validate positive finite limits before target conversion. Deduplicate at
    most the five Standard targets, construct each through existing
    Chatml_compiler_contract.for_target, and charge canonical conversion work,
    encoded bytes, and allocation BEFORE retention. The aggregate identity
    allocation may be at most 40 MiB, with a fixed 16 MiB allocation AND
    encoded ceiling for each distinct target; max_work_steps is at most 1M.
    Callback cancellation/unexpected exceptions propagate unchanged. *)
val create
  :  targets:Chatml.Chatml_compiler_contract.Target.t list
  -> max_identity_bytes:int
  -> max_work_steps:int
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) result

(** Pure comparison lookup; absence never constructs a new target or grants
    upload/native access. A configured mapping requiring an absent target
    rejects at binding time. *)
val find : t -> Chatml.Chatml_compiler_contract.Target.t -> Entry.t option

val max_identity_bytes : t -> int
val max_work_steps : t -> int
