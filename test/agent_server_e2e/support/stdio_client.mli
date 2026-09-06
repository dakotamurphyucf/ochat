open Core

(** Sequential typed protocol client over an interactive stdio child. *)

type t

type response =
  { result : Agent_protocol.Method_result.t
  ; notifications : Agent_protocol.Envelope.t list
  }
[@@deriving sexp]

val create : process:Stdio_process.t -> clock:_ Eio.Time.clock -> t
val process : t -> Stdio_process.t

(** [request t command] sends one request and returns its typed result plus all
    notifications observed before the correlated response. *)
val request : t -> Agent_protocol.Command.t -> (response, Agent_protocol.Error.t) result

(** [initialize t] performs Protocol 1.0 initialization. *)
val initialize
  :  t
  -> ( Agent_protocol.Initialize.Response.t * Agent_protocol.Envelope.t list
       , Agent_protocol.Error.t )
       result

(** [send_raw_line] bypasses envelope construction for framing tests. *)
val send_raw_line : t -> string -> (unit, Agent_protocol.Error.t) result

(** [next_envelope] reads one additional protocol envelope. *)
val next_envelope
  :  t
  -> timeout_seconds:float
  -> (Agent_protocol.Envelope.t, Agent_protocol.Error.t) result
