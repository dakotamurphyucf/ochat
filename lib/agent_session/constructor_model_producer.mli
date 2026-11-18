open! Core

(** One synchronous constructor Model producer's lexical extent, not execution
    authority. Domain confined. Only [with_extent] can record its joined exit;
    initialization retirement does not imply this producer has exited. *)
type t

module Exit : sig
  type kind =
    | Returned
    | Cancelled
    | Raised of exn * Stdlib.Printexc.raw_backtrace

  type completion =
    | Selecting
    | Sample of Recurrence_runtime.Completion.sample
    | Clock_failed of exn * Stdlib.Printexc.raw_backtrace

  type t =
    { kind : kind
    ; completion : completion
    }
end

val create : sample:(unit -> Recurrence_runtime.Completion.sample) -> t

(** Run the actual existing producer once. Record its exit with [Selecting]
    before invoking the sampler, then retain its sample or clock diagnostic.
    Sampling may suspend; [Selecting] is not a usable completion clock pair.
    A sampler failure never replaces the producer result or original exception.
    No cleanup, mailbox call or provider replay runs here. *)
val with_extent : t -> (unit -> 'a) -> 'a

val exit : t -> Exit.t option
