open Core
module Build = Chatml_project_build
module Contract = Chatml.Chatml_compiler_contract
module Captured = Chatml_source

module Limits : sig
  type t

  val create
    :  max_example_bytes:int
    -> max_recomposition_work_steps:int
    -> max_recomposition_input_bytes:int
    -> max_dependency_depth:int
    -> compile:Chatml_compilation.limits
    -> t Or_error.t

  val max_example_bytes : t -> int
end

module Error : sig
  type t =
    | Invalid_selected_entry
    | Incompatible_selected_contract
    | Recomposition of Build.Error.t
    | Compile_failure of Chatml_compilation.error
    | Budget_exceeded
end

module Witness : sig
  type t

  val example : t -> string
  val target : t -> Contract.Target.t
  val base_fingerprint : t -> Captured.Digest.t
  val selected_contract : t -> Captured.Digest.t
  val environment : t -> string
  val original_entry_id : t -> Captured.Id.t
  val original_entry_digest : t -> Captured.Digest.t
  val example_digest : t -> Captured.Digest.t
  val checked_input_fingerprint : t -> Captured.Digest.t
end

type outcome =
  | Checked of Witness.t
  | Inapplicable

(** Explicit Analyze opt-in. Choose at most one curated complete example for
    the actual selected Standard target, replace only the captured entry through
    [Build.Effective.recompose], and run the existing joined non-executing graph
    compiler under bounded policy. Native has no curated example. Every
    compiler/recomposition/resource failure aborts this preparation with a typed
    error; cancellation and unexpected checkpoint exceptions retain identity.
    The linked artifact and temporary Build are discarded. The returned small
    immutable witness is comparison evidence, never compiler authority. *)
val prepare
  :  Build.t
  -> contract:Contract.t
  -> limits:Limits.t
  -> env:Eio_unix.Stdenv.base
  -> checkpoint:(unit -> unit)
  -> (outcome, Error.t) Result.t
