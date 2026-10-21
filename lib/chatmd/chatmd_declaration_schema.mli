open! Core

(** Parser-owned declaration metadata. Validators and authoring suggestions use
    these same finite tables; this module never accepts a declaration on its
    own. In particular a [Helpful] list does not reject other names accepted by
    a permissive legacy parser. No source loading, execution or UI dependency. *)

module Shell_context = Chatmd_shell_schema.Context

module Family : sig
  type t =
    | Plain_tag of Chatmd_ast.tag
    | Script_legacy
    | Script_extension
    | Tool_legacy
    | Tool_extension
    | Tool_shell
    | Tool_read_file
    | Tool_inherited
    | Shell_element of Shell_context.t

  val equal : t -> t -> bool
  val sexp_of_t : t -> Sexp.t
end

module Attribute_names : sig
  type t =
    | Exhaustive of string list
    | Helpful of string list
  [@@deriving sexp_of]

  val names : t -> string list
end

module Choice = Chatmd_shell_schema.Value_choices

(** Exactly the current complete script/tool dispatch in [Prompt], including
    legacy last-value and extension any-value cases. Validators use this
    operation at their dispatch boundary. Nested shell elements require their
    actual ancestor context, not only a tag spelling. *)
val family_of_complete
  :  tag:Chatmd_ast.tag
  -> attributes:Chatmd_ast.attribute list
  -> Family.t

module Script_surface : sig
  type t =
    | Moderator
    | Tool
  [@@deriving equal, sexp_of]

  (** The extension validator's current [kind]/[api] combinations. [None]
      retains its existing invalid-surface diagnostic at the call site. *)
  val of_values : kind:string -> api:string option -> t option

  val kind_names : string list
  val api_names : string list
end

module Extension_tool_binding : sig
  type t =
    | Moderator of string
    | Standalone of
        { script : string
        ; entrypoint : string
        }
  [@@deriving sexp_of]

  (** Exact existing type/reference/entrypoint combinations, before source
      qualification or capability admission. *)
  val of_values
    :  type_name:string
    -> moderator:string option
    -> script:string option
    -> entrypoint:string option
    -> t option

  val type_names : string list
  val entrypoint_names : string list
end

module Prefix : sig
  type issue =
    | Duplicate_attribute of string
    | Invalid_complete_discriminator of string
  [@@deriving sexp_of]

  type t = private
    | Viable of Family.t list
    | Uncertain of Family.t list
    | Malformed of issue
    | Limit_exceeded
  [@@deriving sexp_of]
end

(** For an unfinished tag, return all still-possible parser families in stable
    order. Only completed, decoded discriminator values may narrow the set. A
    partial quote/escape/entity leaves families [Uncertain]. Duplicate names
    in strict declarations or fully closed impossible script discriminators
    are [Malformed] for authoring; duplicate names in permissive or still
    reroutable declarations remain [Uncertain],
    without changing accepted batch parser behavior. More than 128 completed
    attributes, a name/value over 4096 bytes, or 65536 aggregate attribute
    bytes yield [Limit_exceeded] before any sorting or hashing. The bounded
    admission and finite family inventory bound subsequent comparisons. *)
val families_of_prefix
  :  scope:Chatmd_tag_scope.t
  -> tag:Chatmd_ast.tag
  -> completed_attributes:Chatmd_ast.attribute list
  -> uncertain_discriminator:bool
  -> checkpoint:(unit -> unit)
  -> Prefix.t

(** Nested shell tables are keyed by real ancestor context. Unknown or illegal
    nesting yields a helpful empty list, never a guessed exhaustive list. *)
val attributes : Family.t -> Attribute_names.t

module Attribute_candidate : sig
  type applicability =
    | All_viable_families
    | Only of Family.t list
  [@@deriving sexp_of]

  type t

  val name : t -> string
  val applicability : t -> applicability
end

(** Preserve discoverability of family discriminators and specialized forms.
    Family-specific names are conditional candidates; a conditional candidate
    is not a declaration-validity claim. Existing names are excluded. Input
    beyond the same prefix limits yields no candidates. *)
val attributes_for_prefix
  :  Prefix.t
  -> completed_attributes:Chatmd_ast.attribute list
  -> checkpoint:(unit -> unit)
  -> Attribute_candidate.t list

(** Only unconditional whole-value finite enums are [Closed]. A
    [Comma_separated_tokens] result describes allowed individual tokens, not
    the full value. [Freeform] says no value suggestion follows from this
    schema; it does not assert that every string is valid. Validators consume
    the same spelling maps. *)
val choices : Family.t -> attribute:string -> Choice.t option

(** Canonical names from the AST/shell-element vocabularies, filtered by the
    shared lexical placement rule. Fixed inventory and bounded polling. *)
val tag_names : Chatmd_tag_scope.t -> checkpoint:(unit -> unit) -> string list
