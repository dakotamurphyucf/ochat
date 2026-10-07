(** Private selected completion/scalar parsing used by actual meta routes. *)
exception Configuration_required

val require : Inference_client.Execution.t option -> Inference_client.Execution.t
val setting : string -> Jsonaf.t -> Inference.Request.Setting.t

val complete
  :  Inference_client.Execution.t
  -> ?model:string
  -> settings:Inference.Request.Setting.t list
  -> messages:(History_entry.Payload.Role.t * string) list
  -> unit
  -> (string, Inference_client.Execution.Completion_error.t) Result.t

(** Exact complete JSON number, finite and in [0,max]. No prefix parsing/clamp. *)
val score : string -> max:float -> float option
