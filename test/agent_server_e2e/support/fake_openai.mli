open Core

(** Local deterministic OpenAI Responses-compatible SSE endpoint. *)

val start
  :  sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> port:int
  -> marker:string
  -> release:unit Eio.Promise.t
  -> unit
