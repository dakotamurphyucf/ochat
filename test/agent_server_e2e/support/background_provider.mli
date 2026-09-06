open Core

(** Gated local Responses endpoint for real nested model execution. Each request
    remains blocked until explicitly released; requests never reach a provider. *)
type t

val start : sw:Eio.Switch.t -> env:Eio_unix.Stdenv.base -> port:int -> t
val request_count : t -> int
val request_body : t -> index:int -> string
val release : t -> index:int -> unit
