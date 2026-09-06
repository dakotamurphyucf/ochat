(** Gate real nonstreaming Responses requests after consuming their JSON body. *)
type t

type request

type outcome =
  | Summary of string
  | Missing_summary
  | Raw_json of string

val start : sw:Eio.Switch.t -> env:Eio_unix.Stdenv.base -> port:int -> t
val await_request : t -> env:Eio_unix.Stdenv.base -> index:int -> request
val body : request -> Jsonaf.t
val release : request -> outcome -> unit

(** [await_returned request ~env] waits until the handler has constructed its
    response after release. It does not assert delivery to a cancelled client. *)
val await_returned : request -> env:Eio_unix.Stdenv.base -> unit

val request_count : t -> int
