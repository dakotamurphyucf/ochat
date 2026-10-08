open! Core

(** Private bounded RFC6455 client framing over the existing verified TLS flow.
    No connection acquisition, credentials, inference retry or event semantics. *)
module Limits : sig
  type t

  val create
    :  max_frame_bytes:int
    -> max_message_bytes:int
    -> max_write_bytes:int
    -> max_framing_bytes:int
    -> max_fragments:int
    -> max_control_frames:int
    -> t Or_error.t
end

type error =
  | Protocol
  | Framing_limit
  | Limit
  | Closed
[@@deriving equal, sexp_of]

type t

(** [read] returns exactly requested bytes or raises transport I/O failure;
    [write] serializes all bytes or raises. Both belong to the channel switch.
    [random] is cryptographic entropy, never seeded pseudo-random state. *)
val create
  :  limits:Limits.t
  -> read:(int -> string)
  -> write:(string -> unit)
  -> random:(int -> string)
  -> t

(** Reset the cumulative frame/control-overhead budget for one inference. *)
val begin_response : t -> unit

(** One bounded text message, processing interleaved ping/pong/close controls.
    No unbounded queue or spawned fiber. Strict UTF8; extensions/binary reject.
    Control/fragment budgets are per admitted text message, including before it. *)
val read_text : t -> (string, error) Result.t

val write_text : t -> string -> (unit, error) Result.t
val close : t -> unit

(** Primary RFC6455 section4.1 handshake verifier. Header ownership/byte bounds
    belong to existing HTTP reader. Reject unsolicited extensions/subprotocols. *)
val accept : nonce:string -> string

val validate_upgrade
  :  nonce:string
  -> status:int
  -> headers:(string * string) list
  -> (unit, error) Result.t
