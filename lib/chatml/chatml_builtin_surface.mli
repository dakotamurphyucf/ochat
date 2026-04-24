(** Composable builtin and type surfaces for ChatML embedders.

    A surface keeps the runtime installer and the typechecker aligned by
    bundling:

    - top-level globals,
    - builtin modules,
    - host-provided type aliases.

    Different hosts can assemble different capability sets while still
    compiling the same ChatML language core.

    The optional UI-only notification and approval capabilities documented in
    [docs-src/chatml-ui-host-capabilities.md] are intentionally kept outside
    the default non-UI surfaces in this module. *)

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

(** The default builtin surface installed for moderator scripts.

    This layers the moderator capability modules on top of
    {!core_surface}, including:

    - [Item] helpers for structured transcript items, including
      [text], [user_text], [assistant_text], [system_text], [notice],
      [is_user], [is_assistant], [is_system], [is_tool_call], and
      [is_tool_result],
    - [Tool_call] inspectors such as [arg], [arg_string], [arg_bool],
      [arg_array], [is_named], and [is_one_of],
    - [Context] selectors such as [last_item], [last_user_item],
      [last_assistant_item], [last_system_item], [last_tool_call],
      [last_tool_result], [find_item], [items_since_last_user_turn],
      [items_since_last_assistant_turn], [items_by_role], [find_tool], and
      [has_tool],
    - [Turn] item mutation helpers such as [append_item],
      [replace_item], [delete_item], [replace_or_append], and
      [append_notice],
    - [Model] helpers such as [call], [call_text], [call_json], [spawn], and
      [spawn_text],
    - [Process] helpers such as [run],
    - and the structural aliases used by moderator entrypoints such as
      [item] and [context].

    The optional [Ui] and [Approval] capability modules are documented
    separately in [docs-src/chatml-ui-host-capabilities.md] and are not part
    of this default surface. *)
val moderator_surface : surface

(** The UI-only moderator surface for interactive hosts such as [chat_tui].

    This extends {!moderator_surface} with the [Ui] and [Approval]
    modules while leaving the default non-UI moderator surface unchanged. *)
val ui_moderator_surface : surface
