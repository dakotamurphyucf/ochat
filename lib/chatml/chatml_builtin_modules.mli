(** Installation helpers for ChatML builtin values and modules.

    This module bridges the static builtin/type surface used by the
    typechecker with the mutable runtime environment used by the
    evaluator.  Embedders typically interact with the top-level
    convenience functions {!add_surface}, {!create_env_with_surface},
    {!add_global_builtins}, and {!create_default_env}.

    The nested {!module:BuiltinModules} submodule mirrors the historical
    API shape used by existing tests and call sites. *)

open Chatml.Chatml_lang
module Builtin_surface = Chatml.Chatml_builtin_surface

(** Render a runtime value into a stable, human-readable debugging string.

    The output is intended for diagnostics and expect tests, not for
    machine parsing. *)
val value_to_string : value -> string

module BuiltinModules : sig
  (** Install all globals, modules, and type-aligned runtime bindings
      from the provided builtin surface into an existing environment. *)
  val add_surface : env -> Builtin_surface.surface -> unit

  (** Allocate a fresh environment pre-populated with the runtime bindings
      from the supplied builtin surface. *)
  val create_env_with_surface : Builtin_surface.surface -> env

  (** Install the default core ChatML prelude into an existing
      environment. *)
  val add_global_builtins : env -> unit

  (** Allocate a fresh environment populated with the default core ChatML
      prelude. *)
  val create_default_env : unit -> env
end

(** Alias for {!BuiltinModules.add_surface}. *)
val add_surface : env -> Builtin_surface.surface -> unit

(** Alias for {!BuiltinModules.create_env_with_surface}. *)
val create_env_with_surface : Builtin_surface.surface -> env

(** Alias for {!BuiltinModules.add_global_builtins}. *)
val add_global_builtins : env -> unit

(** Alias for {!BuiltinModules.create_default_env}. *)
val create_default_env : unit -> env
