open Core
module Prepared = Chatml_prepared_program
module Captured = Chatml_source

(** Pure one-source revision of an already admitted inventory. Every location
    anchored to the edited source is rebased only when wholly inside the exact
    prefix. The original graph remains owned by its caller. No source lookup,
    resolver, inference or runtime authority is introduced. *)
val create
  :  Prepared.t
  -> original:Captured.t
  -> end_byte:int
  -> max_input_bytes:int
  -> checkpoint:(unit -> unit)
  -> (Prepared.t, unit) Result.t
