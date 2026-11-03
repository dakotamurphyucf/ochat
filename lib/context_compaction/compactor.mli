open! Core

(** Same explicit o200k estimate used for retained compaction text. Estimates
    local text tokens; does not claim provider/image/wire accounting. *)
val estimated_text_tokens : string -> int

(** Transactional canonical compaction using the explicitly selected inference
    execution. Original policy/reminder entries keep complete payloads and IDs;
    exactly one Authored user reminder is allocated after summarization and the
    local token budget check succeed. No provider DTO conversion or ambient
    model/credential selection. [env] only supplies configuration; no automatic retry occurs.
    Token estimates use semantic presentation plus per-item overhead, not exact
    provider/image accounting. Cancellation and strict callback errors propagate. *)
val compact_entries
  :  inference:Inference_client.Execution.t
  -> allocator:History_entry.Allocator.t
  -> env:Eio_unix.Stdenv.base option
  -> history:History_entry.t list
  -> (History_entry.t list, exn) Result.t

(** Explicit normalized guidance and exact framed-text local token estimate.
    Reserve before relevance/summarizer selection and final allocator admission.
    The caller retains genuine source/owner lifetime and guards the execution;
    no guidance enters retained history or receives an ID. *)
val compact_entries_with_guidance
  :  guidance:Inference.Request.Guidance.t option
  -> reserved_input_tokens:int
  -> inference:Inference_client.Execution.t
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
