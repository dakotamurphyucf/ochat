module Res = Openai.Responses

module Kind : sig
  type t =
    | Function
    | Custom
end

(** True for the registered built-in fork implementation. A same-name
    replacement retains its own implementation and gains no fork ownership. *)
val is_builtin_fork : Ochat_function.t -> bool

val fork_custom_error : string

(** An invocation-local native binding supplied by the host that owns its
    admission. The original selected implementation retains its identity.
    [prepare_invocation] checks actual admitted authority at entry and preserves
    the driver observers while attaching any effect checks needed by the tool. *)
module Native : sig
  type t

  val create
    :  Ochat_function.t
    -> prepare_invocation:(Ochat_function.Invocation.t -> Ochat_function.Invocation.t)
    -> t

  (** Invoke the selected implementation without driver substitution. *)
  val run : t -> Ochat_function.runner
end

(** Execute the selected binding, or the driver-local binding when no host
    dispatch owns the call. Only the built-in fork uses [on_fork]. The
    host's invocation preparation applies to the actual selected executor
    after Started, including after driver substitution. Direct callers can select
    an existing [runner] explicitly. Supply only one of [native] and [runner].
    Without a selected binding, standalone drivers retain their built-in fork
    unless their tool table provides an explicit replacement. *)
val run_tool
  :  kind:Kind.t
  -> name:string
  -> payload:string
  -> call_id:string
  -> tool_tbl:(string, Ochat_function.runner) Base.Hashtbl.t
  -> on_fork:
       (invocation:Ochat_function.Invocation.t
        -> call_id:string
        -> arguments:string
        -> Res.Tool_output.Output.t)
         option
  -> ?native:Native.t
  -> ?runner:Ochat_function.runner
  -> ?before_effect:(unit -> unit)
  -> ?inference_parent:Transcript.Scope.parent
  -> ?on_tool_execution:(Tool_execution_event.t -> unit)
  -> ?on_execution_event:(Tool_execution_event.t -> unit)
  -> unit
  -> Openai.Responses.Tool_output.Output.t

val call_item
  :  kind:Kind.t
  -> name:string
  -> payload:string
  -> call_id:string
  -> id:string option
  -> Res.Item.t

val function_call_output
  :  call_id:string
  -> output:Res.Tool_output.Output.t
  -> Res.Function_call_output.t

val custom_tool_call_output
  :  call_id:string
  -> output:Res.Tool_output.Output.t
  -> Res.Custom_tool_call_output.t

val output_item
  :  kind:Kind.t
  -> call_id:string
  -> output:Res.Tool_output.Output.t
  -> Res.Item.t
