open! Core

(** Selected no-tool summarization. No model/key lookup or implicit offline stub.
    Canonical history is rendered semantically; unknown provider items have an
    explicit placeholder, never opaque replay bytes. Grouping retains related
    calls/results together for the compactor's relevance selection. *)
val grouped_items : History_entry.t list -> History_entry.t list list

val render_transcript : History_entry.t list -> string

exception Failed of Inference_client.Execution.Completion_error.t

(** Inherits the selected target settings without adding a fixed output limit.
    Exactly one request is dispatched. Typed inference failures return an error
    without retry or bisection because completion errors carry no proof that
    submission did not occur. Cancellation, strict callback failures and
    unexpected exceptions propagate unchanged. [env] is retained for caller
    compatibility; it never selects a backend. *)
val summarise
  :  inference:Inference_client.Execution.t
  -> relevant_items:History_entry.t list
  -> env:Eio_unix.Stdenv.base option
  -> (string, exn) Result.t

module For_testing : sig
  val render_transcript : History_entry.t list -> string

  val summarise_with
    :  request:
         (History_entry.t list
          -> (string, Inference_client.Execution.Completion_error.t) Result.t)
    -> relevant_items:History_entry.t list
    -> (string, exn) Result.t
end
