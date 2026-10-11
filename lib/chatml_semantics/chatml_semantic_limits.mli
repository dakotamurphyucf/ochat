open Core
module Recorder = Chatml_typechecker.Semantic
module Frozen = Chatml_typechecker.Type_snapshot

(** One positive immutable plan; structural phase capacities are cumulative per
    invocation, never reset per module. Work counts every supplied checkpoint
    across fingerprint/check/freeze/publication. No retained checkpoint. *)
type t

val create
  :  max_input_bytes:int
  -> recording:Recorder.Limits.t
  -> type_graph:Frozen.Limits.t
  -> max_published_facts:int
  -> max_published_edges:int
  -> max_published_label_bytes:int
  -> max_work_steps:int
  -> t Or_error.t

val max_input_bytes : t -> int
val recording : t -> Recorder.Limits.t
val type_graph : t -> Frozen.Limits.t
val max_published_facts : t -> int
val max_published_edges : t -> int
val max_published_label_bytes : t -> int
val max_work_steps : t -> int
