(** Composable builtin and type surfaces for ChatML embedders.

    A surface keeps the runtime installer and the typechecker aligned by
    bundling:

    - top-level globals,
    - builtin modules,
    - host-provided type aliases.

    Different hosts can assemble different capability sets while still
    compiling the same ChatML language core. *)

(** Host-provided type alias available during typechecking. *)
type builtin_type_alias =
  { name : string
  ; body : Chatml_builtin_spec.ty
  }

(** Complete builtin surface installed into both the runtime environment
    and the initial type environment. *)
type surface =
  { globals : Chatml_builtin_spec.builtin list
  ; modules : Chatml_builtin_spec.builtin_module list
  ; type_aliases : builtin_type_alias list
  }

(** Empty surface with no globals, modules, or aliases. *)
val empty : surface

(** Merge two surfaces, deduplicating entries by name and keeping the
    left-most definition on conflicts. *)
val merge : surface -> surface -> surface

(** Convenience constructor for a surface made only from builtin modules. *)
val of_modules : Chatml_builtin_spec.builtin_module list -> surface

(** The default core ChatML surface used by the evaluator and typechecker
    outside moderator runtimes. *)
val core_surface : surface

(** A richer surface intended for moderator runtimes.

    This layers the moderator capability modules on top of
    {!core_surface}, including:

    - [Item] helpers for structured transcript items,
    - [Turn] item mutation helpers such as [append_item],
      [replace_item], and [delete_item],
    - and the structural aliases used by moderator entrypoints such as
      [item] and [context]. *)
val moderator_surface : surface
