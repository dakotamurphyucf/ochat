open! Core

(** One pure decoder for the existing agent_prompt_v1 recipe payload. The
    original Jsonaf tree, including unknown members, remains available to the
    admitting owner. Parsing does not fetch prompts, credentials or history. *)
type t

val recipe_name : string

(** Preserve the conversation recipe's existing field grammar, first-field
    lookup, defaults and exact error strings. This syntax-only operation has no
    developer limit and changes no conversation admission behavior. *)
val decode : Jsonaf.t -> (t, string) result

(** Developer admission validates the WHOLE raw tree and compact encoded byte
    count before decoding. The physical owner also applies its actual Store
    per-job and total-document caps. No capacity or durable mutation occurs
    here. *)
val admit
  :  Jsonaf.t
  -> limits:Document_schema.Limits.t
  -> (t, Agent_protocol.Error.t) result

val original : t -> Jsonaf.t
val prompt : t -> string
val input : t -> string
val is_local : t -> bool
val history_compaction : t -> bool option
val nested_prompt_id : t -> string option
