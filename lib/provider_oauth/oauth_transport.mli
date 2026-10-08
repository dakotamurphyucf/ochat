open! Core

module Error : sig
  type t =
    | Closed
    | Connection
    | Tls
    | Invalid_http
    | Body_limit
    | Timeout
  [@@deriving equal, sexp_of]
end

type t

type endpoint =
  | User_code
  | Device_poll
  | Token
  (** Fixed verified HTTPS only, redirects unsupported. Per-call switch owns all
    sockets. submit marker fires BEFORE first request-byte write, conservatively. *)

val create : net:_ Eio.Net.t -> clock:_ Eio.Time.Mono.t -> (t, Error.t) result
val close : t -> unit

val post
  :  t
  -> endpoint
  -> content_type:string
  -> body:string
  -> on_possible_submission:(unit -> unit)
  -> (int * string, Error.t) result

(** Shared private bounded HTTP syntax for the loopback callback. *)
exception Transport_error of Error.t

val line : Eio.Buf_read.t -> string
val headers : Eio.Buf_read.t -> string String.Map.t

val scripted
  :  clock:_ Eio.Time.Mono.t
  -> (endpoint
      -> body:string
      -> on_possible_submission:(unit -> unit)
      -> (int * string, Error.t) result)
  -> t

val parse_response : string -> (int * int, Error.t) result
