open Core

(** Runtime adapter only: callbacks must belong to the currently executing
    actor owner and captured tool authority. Registration grants no authority. *)
type handlers =
  { start_tool : name:string -> input:Jsonaf.t -> (Agent_protocol.Id.Job.t, string) result
  ; start_script : Jsonaf.t -> (Agent_protocol.Id.Job.t, string) result
  ; get : Agent_protocol.Id.Job.t -> (Jsonaf.t, string) result
  ; cancel : Agent_protocol.Id.Job.t -> (unit, string) result
  ; rollback_start : Agent_protocol.Id.Job.t -> unit
  }

(** Start operations reserve only; the actor publishes them with the owning
    transaction. Catch rollback calls rollback_start before recovery. Get/cancel
    are immediate host operations; cancellation of existing work is not undone
    by Task.catch. Callbacks own liveness, quotas, result projection and cleanup. *)
val install
  :  ?control:Chatml.Chatml_lang.execution_control
  -> handlers:handlers
  -> Chatml_host_runtime.runtime_config
  -> Chatml_host_runtime.runtime_config

(** Decode host-returned IDs from surviving start effects and return the remaining
    effects unchanged. Invalid/duplicate IDs fail; request JSON is never used to
    correlate returned starts. The actor must still validate ownership/selection. *)
val split_starts
  :  Chatml.Chatml_lang.eff list
  -> (Agent_protocol.Id.Job.t list * Chatml.Chatml_lang.eff list, string) result
