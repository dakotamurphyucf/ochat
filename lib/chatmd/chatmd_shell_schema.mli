open! Core

(** Attribute and finite-value metadata for the actual nested shell, read-file
    and shell-tool declaration validators. A context carries the complete
    lexical ancestor chain; a tag alone cannot identify a [path], [arg] or
    matcher declaration. This module does not validate source or authorize a
    capability. *)

module Root : sig
  type t =
    | Shell_access
    | Tool_shell
    | Tool_read_file
  [@@deriving compare, equal, sexp_of]
end

(** The declaration parser consumes these typed spelling/constructor maps.
    Suggestions project their names from the same maps. *)
module Enum : sig
  val bool : (string * bool) list
  val merge : (string * Chatmd_shell_spec.Shell_spec.merge) list
  val sandbox : (string * Chatmd_shell_spec.Shell_spec.sandbox) list
  val path_position : (string * Chatmd_shell_spec.Shell_spec.path_position) list

  val environment_inherit
    : (string * Chatmd_shell_spec.Shell_spec.environment_inherit) list

  val platform : (string * Chatmd_shell_spec.Shell_spec.platform) list
  val confinement : (string * Chatmd_shell_spec.Shell_spec.confinement) list
  val process_effect : (string * Chatmd_shell_spec.Shell_spec.process_effect) list
  val lifecycle : (string * Chatmd_shell_spec.Shell_spec.lifecycle) list
  val hook_failure : (string * Chatmd_shell_spec.Shell_spec.hook_failure) list
  val hook_protocol : (string * Chatmd_shell_spec.Shell_spec.hook_protocol) list
  val policy_action : (string * Chatmd_shell_spec.Shell_spec.policy_action) list
  val approval_scope : (string * Chatmd_shell_spec.Shell_spec.approval_scope) list
  val approval_provider : (string * Chatmd_shell_spec.Shell_spec.approval_provider) list

  val approval_unavailable
    : (string * Chatmd_shell_spec.Shell_spec.approval_unavailable) list

  val interceptor_phase : (string * Chatmd_shell_spec.Shell_spec.interceptor_phase) list
  val audit_format : (string * Chatmd_shell_spec.Shell_spec.audit_format) list
  val audit_content : (string * Chatmd_shell_spec.Shell_spec.audit_content) list
  val audit_failure : (string * Chatmd_shell_spec.Shell_spec.audit_failure) list

  val tool_arguments_mode
    : (string * Chatmd_shell_spec.Shell_tool_spec.arguments_mode) list
end

module Value_choices : sig
  type t =
    | Closed of string list
    | Comma_separated_tokens of string list
    | Freeform
  [@@deriving sexp_of]
end

module Reviewer_kind : sig
  type t =
    | Ui
    | Chatml
    | Model
    | Executable

  val names : string list
  val of_string : string -> t option
end

module Analyzer_kind : sig
  type t =
    | Chatml
    | Executable

  val names : string list
  val of_string : string -> t option
end

val reviewer_strategy : string

module Context : sig
  type t

  (** Nearest completed structured lexical root and its entered
      shell elements. A context is absent for an extension tool or non-shell
      ancestor. If a tool has both shell and read-file lexical hints, existing
      shell precedence selects the shell root. Declaration validity remains
      parser-owned. *)
  val of_scope
    :  Chatmd_tag_scope.t
    -> element:Chatmd_shell_spec.Shell_element.t
    -> checkpoint:(unit -> unit)
    -> t option

  val root : t -> Root.t
  val ancestors : t -> Chatmd_shell_spec.Shell_element.t list
  val element : t -> Chatmd_shell_spec.Shell_element.t
  val sexp_of_t : t -> Sexp.t
end

(** Exact existing allowed names at a legal parser context. [None] means no
    exact validator metadata for this nesting; it is not an empty allowlist.
    The authoring producer must avoid claiming acceptance in that case. *)
val allowed : Context.t -> string list option

(** Narrow parser adapter for its already constructed diagnostic path. The
    call sites use this instead of a second copy of the literal allowlist.
    Raises only for an internal missing schema entry, never for source input. *)
val allowed_for_parser_path_exn : root:Root.t -> path:string list -> string list

(** An unconditional finite enum or comma-separated enum-token vocabulary at
    this context. [None] also covers values whose validator depends on a sibling
    discriminator; callers must not infer that the value is unrestricted. *)
val choices : Context.t -> attribute:string -> Value_choices.t option
