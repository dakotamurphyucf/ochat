open! Core

(** Selected no-tool summarization. No model/key lookup or implicit offline stub.
    Canonical history is rendered semantically; unknown provider items have an
    explicit placeholder, never opaque replay bytes. Policy and reminders remain
    shared when retry exhaustion triggers one ordered, call-group-aware split. *)
val grouped_items : History_entry.t list -> History_entry.t list list

val render_transcript : History_entry.t list -> string

exception Failed of Inference_client.Execution.Completion_error.t

(** Protocol response failures receive three total attempts, then one bisection.
    Other typed inference failures return an error without retry. Cancellation,
    strict callback failures and unexpected exceptions propagate unchanged.
    [env] supplies only the retry clock; omission never selects a backend. *)
val summarise
  :  inference:Inference_client.Execution.t
  -> relevant_items:History_entry.t list
  -> env:Eio_unix.Stdenv.base option
  -> (string, exn) Result.t

module For_testing : sig
  val render_transcript : History_entry.t list -> string

  val summarise_with
    :  sleep:(float -> unit)
    -> request:
         (History_entry.t list
          -> (string, Inference_client.Execution.Completion_error.t) Result.t)
    -> relevant_items:History_entry.t list
    -> (string, exn) Result.t
end
