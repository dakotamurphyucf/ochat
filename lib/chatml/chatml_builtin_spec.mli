(** Static specification for ChatML builtin values, modules, and shared
    moderator-runtime structural types.

    This module is the central definition site for:

    - builtin type schemes used by the typechecker,
    - builtin runtime implementations installed into environments,
    - reusable structural aliases such as {!val-json_ty} and
      moderator-oriented record types,
    - curated builtin module sets used to assemble surfaces.

    Most embedders will use {!Chatml_builtin_surface} rather than
    constructing values from this module directly, but the types exposed
    here are shared by the surface builder, the builtin installer, and
    the typechecker. *)

open Core
open Chatml_lang

(** Row descriptions used by builtin record and variant types. *)
type row =
  | TRow_empty
  | TRow_var of string
  | TRow_extend of (string * ty) list * row

(** The builtin type language mirrored by {!Chatml_typechecker}. *)
and ty =
  | TVar of string
  | TCon of string * ty list
  | TInt
  | TFloat
  | TBool
  | TString
  | TUnit
  | TArray of ty
  | TRef of ty
  | TTuple of ty list
  | TRecord of row
  | TVariant of row
  | TFun of ty list * ty
  | TMu of string * ty
  | TRec_var of string

(** A top-level builtin value binding along with its type scheme. *)
type builtin =
  { name : string
  ; scheme : ty
  ; impl : value list -> value
  }

(** A builtin module installed as a record-like runtime value. *)
type builtin_module =
  { name : string
  ; exports : builtin list
  }

(** Compute the record type corresponding to a builtin module's exports. *)
val module_scheme : builtin_module -> ty

(** Render a runtime value into a stable debugging string shared by tests
    and host tooling. *)
val value_to_string : value -> string

(** Task type constructor helper. *)
val task_ty : ty -> ty

(** The core builtin globals installed into the default environment. *)
val builtins : builtin list

(** Legacy/core builtin modules shared by the default ChatML prelude. *)
val modules : builtin_module list

(** Explicit name for the core builtin module set used by
    {!Chatml_builtin_surface.core_surface}. *)
val core_modules : builtin_module list

(** Capability modules intended for moderator runtimes, such as
    [Log], [Item], [Turn], [Tool], [Model], [Schedule], and [Runtime].

    The [Item] module provides helpers for inspecting and constructing
    structured transcript items, while [Turn] exposes item-oriented
    mutation helpers such as [append_item], [replace_item], and
    [delete_item] alongside legacy message aliases. *)
val moderator_modules : builtin_module list

(** Structural builtin type representing JSON values. *)
val json_ty : ty

(** Structural moderator-runtime record type for transcript items.

    Moderator scripts receive [ctx.items] values of this shape and can
    pass them back through [Turn.*] mutation helpers. *)
val item_ty : ty

val tool_desc_ty : ty
val tool_call_ty : ty
val tool_result_ty : ty
val context_ty : ty
