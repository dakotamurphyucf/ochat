(** Built-in standard library for the {e ChatML} interpreter.

    This module registers a minimal set of run-time primitives
    ("built-ins") inside a {!Chatml.Chatml_lang.env}.  The exported
    function {!BuiltinModules.add_global_builtins} mutates the provided
    environment in place so that user programs can access:

    {ul
    {- Basic printing utilities ([print] and [to_string]).}
    {- Array helper ([length]).}
    {- Convenience conversions ([num2str] and [bool2str]).}
    {- Arithmetic operators ([+], [-], [*], [/]).}
    {- Comparison operators ([<], [>], [<=], [>=], [==], [!=]).}}

    The implementation is intentionally small – it exists mostly to
    exercise the dynamic value representation of ChatML and to make the
    REPL usable during development.

    {2 Usage}

    {[
      open Chatml.Chatml_lang

      let env = create_env () in
      Chatml.Chatml_builtin_modules.BuiltinModules.add_global_builtins env;

      match find_var env "to_string" with
      | Some (VBuiltin f) ->
          let VString s = f [ VInt 6 ] in
          assert (String.equal s "6")
      | _ -> assert false
    ]}

    All helpers perform dynamic run-time checks and raise [Failure] with
    a descriptive message when the arguments do not satisfy the
    expected shape.
*)

open Core
open Chatml.Chatml_lang
module Builtin_spec = Chatml.Chatml_builtin_spec
(* Provides [value], [env], [set_var] … *)

(* -------------------------------------------------------------------------- *)
(* Shared helpers                                                              *)
(* -------------------------------------------------------------------------- *)

(** [value_to_string v] converts runtime value [v] to a human-readable
      string.  The representation is stable and intended primarily for
      debugging and unit-testing – it is {b not} meant to be parsed
      back by the interpreter.

      - Arrays are printed using the OCaml literal syntax {[| … |]}.
      - Records are rendered as {[{ field = value; … }]}.
      - References show their dereferenced content as
        {e ref}("value").
      - Function, module and builtin closures are abstracted as
        placeholder strings.
  *)
let value_to_string = Builtin_spec.value_to_string

module BuiltinModules = struct
  (** [add_global_builtins env] populates [env] with the standard
      library of ChatML.

      The function performs {b in-place} mutation: callers must pass a
      fresh environment or be prepared for the existing bindings to be
      overwritten.

      The concrete builtin set is defined centrally in
      {!module:Chatml.Chatml_builtin_spec}.  This function merely
      installs those definitions into the mutable top-level runtime
      environment.  All operations raise [Failure] on arity or type
      mismatch.
  *)
  let add_global_builtins (env : env) =
    List.iter Builtin_spec.builtins ~f:(fun builtin ->
      set_var env builtin.name (VBuiltin builtin.impl))
  ;;

  (** [create_default_env ()] allocates a fresh top-level environment and
      installs the full built-in ChatML prelude into it.

      This is the recommended bootstrap helper for embedders that want a
      ready-to-run interpreter environment whose runtime bindings stay in
      sync with the type-checker's builtin specification. *)
  let create_default_env () : env =
    let env = create_env () in
    add_global_builtins env;
    env
  ;;
  (* end of add_global_builtins body *)
end
