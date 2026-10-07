open! Core

(** Transactional canonical compaction using the explicitly selected inference
    execution. Original policy/reminder entries keep complete payloads and IDs;
    exactly one Authored user reminder is allocated after summarization and the
    local token budget check succeed. No provider DTO conversion or ambient
    model/credential selection. [env] only supplies configuration and retry clock.
    Token estimates use semantic presentation plus per-item overhead, not exact
    provider/image accounting. Cancellation and strict callback errors propagate. *)
val compact_entries
  :  inference:Inference_client.Execution.t
  -> allocator:History_entry.Allocator.t
  -> env:Eio_unix.Stdenv.base option
  -> history:History_entry.t list
  -> (History_entry.t list, exn) Result.t

module For_testing : sig
  val process_current_entries
    :  History_entry.t list
    -> History_entry.t list * History_entry.t list * History_entry.t list

  val compact_entries_with
    :  summarise:
         (relevant_items:History_entry.t list
          -> env:Eio_unix.Stdenv.base option
          -> (string, exn) Result.t)
    -> allocator:History_entry.Allocator.t
    -> env:Eio_unix.Stdenv.base option
    -> history:History_entry.t list
    -> (History_entry.t list, exn) Result.t

  val compact_entries_configured
    :  config:Config.t
    -> summarise:
         (relevant_items:History_entry.t list
          -> env:Eio_unix.Stdenv.base option
          -> (string, exn) Result.t)
    -> allocator:History_entry.Allocator.t
    -> env:Eio_unix.Stdenv.base option
    -> history:History_entry.t list
    -> (History_entry.t list, exn) Result.t

  val select_relevant
    :  score:(string -> float)
    -> Config.t
    -> History_entry.t list
    -> History_entry.t list

  val estimated_tokens : History_entry.t list -> int
end
