open Core

type failure =
  | Evidence_mismatch
  | Stale_source

type t

val create
  :  Chatml_semantics.Provisional.t
  -> full_source:Chatml_source.t
  -> checkpoint:(unit -> unit)
  -> (t, failure) Result.t

val full_location
  :  t
  -> Chatml_source.Location.t
  -> checkpoint:(unit -> unit)
  -> (Chatml_source.Location.t option, failure) Result.t
