open Core
module Lang = Chatml.Chatml_lang
module Linked = Chatml_linked_program
module Surface = Chatml.Chatml_builtin_surface

module Error : sig
  module Kind : sig
    type t =
      | Entrypoint_selection
      | Runtime_surface
      | Invalid_linkage
      | Runtime
    [@@deriving equal, sexp_of]
  end

  (** A finite classification plus one bounded canonical diagnostic. No rendered
      duplicate message, original surface, complete inventory or control. *)
  type t

  val kind : t -> Kind.t
  val diagnostic : t -> Chatml_diagnostic.t
end

(** Execution-local evaluated entry and selected binding values. No global cache,
    second interpreter, inference engine or retained Linked/Prepared inventory.
    Contains mutable language values: never share across independent executions,
    sessions or domains. Persistent owners serialize access. *)
type t

(** Validate all requested names against frozen compiled descriptors and compare
    the caller's ACTUAL static runtime surface against Linked.static_contract
    before any initializer. At least one descriptor is required; compile-only
    artifacts cannot implicitly infer requirements or select dependencies.

    Initialize Linked.runtime_order exactly once, including entry last, with
    fresh builtin tables per unit and a completed map restricted to that unit's
    direct Value_reference dependencies. Open/type-only edges stay inert.
    Body evaluation uses Eval.eval_captured_program. Checked public visibility
    is selected before insertion into a local canonical completed map. The
    entry's private environment is retained for compiled local host bindings;
    companion visibility applies only to external reads.

    All units/installers/selection share [control]. Metadata validation and
    graph/map walks use its diagnostic checkpoint; expression/builtin work
    charges existing fuel. Account runtime container allocation under that same
    control. No per-module fresh budget/depth scope. A persistent owner passes
    runner_control inside run_scoped, not an expiring one-off control.

    On rejection/failure/cancellation, publish no [t], stop later initializers
    and discard private map construction. Prior native effects are not rolled
    back; a host callback which deliberately retained a value remains its owner.
    Native implementations are caller-authorized and must honor their declared
    schemes. Static equality neither authenticates implementations nor grants
    operation/tool authority. Cancellation/control exceptions propagate.
    No discovery, filesystem, provider, task interpretation or event execution. *)
val instantiate
  :  Linked.t
  -> surface:Surface.surface
  -> entrypoints:string list
  -> ?control:Lang.execution_control
  -> unit
  -> (t, Error.t) Result.t

(** Borrowed runtime environment for the existing Host.session implementation.
    This is trusted embedding access, not a source-visible configured fallback.
    Its source/control/dependency map are the defining entry's immutable owners.
    Do not mutate or relabel it; no importer environment is accepted. *)
val entry_env : t -> Lang.env

(** Only names admitted by instantiate are available. Imported descriptors read
    their canonical completed module/member path; locals use private entry
    bindings. No new inference, initialization or arbitrary-name fallback. *)
val binding : t -> name:string -> Lang.value option

val entry_source : t -> Chatml_source.t

(** Incremental, epoch-owned REPL module registry; ordinary [instantiate]
    retains its fresh selected-entry behavior. *)
module Repl_modules : module type of Chatml_repl_modules
