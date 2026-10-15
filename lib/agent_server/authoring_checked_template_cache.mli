open Core
module Ledger = Chatml_authoring_context
module Witness = Authoring_checked_template.Witness

module Error : sig
  type t =
    | Invalid_binding
    | Capacity_limit
    | Retained_byte_limit
end

module Binding : sig
  type t

  (** Actual generation plus the existing validated Overlay receipt or
      genuinely assessed Script, never a wire context label. Project admission
      compares the receipt's Inputs identity/generation and exact selected
      Build; that receipt itself came from the shared effective-closure gate.
      The resulting value retains only bounded comparison data, not the Build,
      parser graph or a grant. *)
  val project
    :  inputs:Ledger.Inputs.t
    -> receipt:Chatml_authoring_effective.Overlay.t
    -> witness:Witness.t
    -> (t, Error.t) result

  val document
    :  inputs:Ledger.Document.Inputs.t
    -> script:Chatmd_authoring_analysis.Script.t
    -> witness:Witness.t
    -> (t, Error.t) result
end

type t
type row

(** One immutable Core-owned latest-per-context witness cache. No new owner or
    authority: the Core gate compares the borrowed cache head before replacing
    it with a prepared successor. [transition] evicts closed/revised contexts.
    Only the example's exact retained bytes count toward this cache allowance;
    the wider source and compiler work have their own admission limits. *)
val create : max_rows:int -> max_retained_bytes:int -> t Or_error.t

val retained_count : t -> int
val retained_bytes : t -> int
val prepare : t -> binding:Binding.t -> (t, Error.t) result
val retire : t -> identity:Ledger.Identity.t -> t
val transition : t -> contexts:Ledger.t -> t

val current
  :  t
  -> identity:Ledger.Identity.t
  -> generation:Ledger.Generation.t
  -> target:Agent_protocol.Authoring.Analysis.Target.selector
  -> row option

val witness : row -> Witness.t

val matches_project
  :  row
  -> inputs:Ledger.Inputs.t
  -> receipt:Chatml_authoring_effective.Overlay.t
  -> bool

val matches_document
  :  row
  -> inputs:Ledger.Document.Inputs.t
  -> script:Chatmd_authoring_analysis.Script.t
  -> bool
