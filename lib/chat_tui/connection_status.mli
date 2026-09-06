open! Core

(** Client-local daemon connection state. This state is presentation-only and
    is never persisted into an agent session. *)

type phase =
  | Connected
  | Reconnecting of { attempt : int }
  | Disconnected
  | Failed of Agent_protocol.Error.t
[@@deriving sexp]

type t =
  { phase : phase
  ; changed_at : Agent_protocol.Timestamp.t
  }
[@@deriving sexp]

val connected : unit -> t
val reconnecting : attempt:int -> t
val disconnected : unit -> t
val failed : Agent_protocol.Error.t -> t
