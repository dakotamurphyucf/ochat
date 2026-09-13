open Core

(** Local deterministic OpenAI Responses-compatible SSE endpoint. *)

(** A complete empty response event for offline streaming fixtures. *)
val completed_event : Jsonaf.t

val start
  :  sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> port:int
  -> marker:string
  -> release:unit Eio.Promise.t
  -> unit
