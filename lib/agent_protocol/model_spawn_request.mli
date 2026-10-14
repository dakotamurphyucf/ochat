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
val history_compaction : t -> bool option
val nested_prompt_id : t -> string option
