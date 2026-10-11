module Captured = Chatml_source
module Prepared = Chatml_prepared_program

(** Immutable selected input/environment identity, no source inventory or
    contract implementation. Only construction trusts the real driver-computed
    selected contract fingerprint. *)
type t

val create : Prepared.t -> selected_contract:Captured.Digest.t -> t
val input_fingerprint : t -> Captured.Digest.t
val required_contract : t -> Captured.Digest.t
val selected_contract : t -> Captured.Digest.t
val environment : t -> Prepared.Environment.t
val entry : t -> Captured.Id.t
val semantic_format : t -> string
