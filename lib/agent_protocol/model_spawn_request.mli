(** Pure shared decoder for the existing [agent_prompt_v1] recipe. Parsing does
    not fetch prompts, credentials or history and grants no model execution. *)
open! Core

type t

val recipe_name : string

(** Preserve the conversation recipe's exact field grammar, first-field
    lookup, defaults and error strings. This syntax-only operation has no
    developer limit and changes no conversation admission behavior. *)
val decode : Jsonaf.t -> (t, string) result

(** Developer admission validates the WHOLE original tree, unique object keys
    and encoded byte count before decoding. Unlike syntax-only [decode], it
    rejects duplicate keys. The Store separately applies its per-job and
    whole-document caps; no capacity or durable mutation occurs here. *)
val admit : Jsonaf.t -> limits:Document_schema.Limits.t -> (t, Protocol_error.t) result

val original : t -> Jsonaf.t
val prompt : t -> string
val input : t -> string
val is_local : t -> bool

module Source_mode : sig
  type t =
    | File
    | Captured
  [@@deriving equal, sexp_of]
end

(** Optional [source_mode] is part of the ONE shared grammar: ["file"] is the
    default, ["captured"] must be explicit and requires [is_local=true]. An
    unknown spelling/type rejects in [decode] before any source effect.
    Existing payloads retain File behavior. A conversation executor without
    captured selected-source custody explicitly refuses Captured rather than
    ignoring it; the developer selected executor may use retained inventory.
    File checks the current original lease even when captured bytes exist.
    The value is a request, not source authority. *)
val source_mode : t -> Source_mode.t

val history_compaction : t -> bool option
val nested_prompt_id : t -> string option
