open! Core

module Limits : sig
  type t

  val create
    :  max_input_bytes:int
    -> max_parse_calls:int
    -> max_tokens:int
    -> max_depth:int
    -> t Or_error.t

  val input_bytes : t -> int
end

(** Invocation-local parser budget for one Root and all its raw imports.
    Never retained in a parsed result or input. *)
type t

val create : Limits.t -> checkpoint:(unit -> unit) -> t
val checkpoint : t -> unit

(** Preserve any tighter host bound and cap referenced script/schema reads too. *)
val bound_loader : t -> Source_loader.t -> Source_loader.t

(** Poll/check actual expanded markup depth before descending. Imports disappear
    from the output are checked by the import poll instead; all retained elements
    (including retained imports) count toward the combined containing depth. *)
val check_expansion : t -> depth:int -> Chatmd_ast.node -> unit

(** Intern exact repeated inputs by Role and logical path within this bounded
    invocation. Changed bytes create a new immutable input, never silently reuse
    an older revision. All live reads and parse/token charges still occur.
    At most [max_parse_calls] new input revisions are admitted. *)
val input
  :  t
  -> role:Chatmd_parser_input.Role.t
  -> path:string
  -> authored:string
  -> effective:string
  -> Chatmd_parser_input.t

(** The next parse must be available before reading an imported input.
    Uses the existing bounded source adapter; no native fallback is added. *)
val read_import
  :  t
  -> Source_loader.t
  -> Source_loader.source
  -> (string, string) Result.t

val parse
  :  t
  -> ?check_token:(Chatmd_tokens.token -> unit)
  -> Chatmd_parser_input.t
  -> Chatmd_located_ast.t list
