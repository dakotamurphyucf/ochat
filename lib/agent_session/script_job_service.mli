open Core

(** Injected actor/scheduler operations. Implementations enforce live ownership,
    current generation and shared capacity; stage must transfer its reservation
    atomically or release it on failure. Get returns an internal record; the
    service checks its captured capability pins before projecting a script result. *)
type host =
  { stage :
      Agent_protocol.Job.launch_owner
      -> Chat_response.Background_request.t
      -> (Agent_protocol.Job.t, Agent_protocol.Error.t) result
  ; select :
      Agent_protocol.Job.launch_owner
      -> Agent_protocol.Id.Job.t list
      -> (unit, Agent_protocol.Error.t) result
  ; abort : Agent_protocol.Job.launch_owner -> Agent_protocol.Id.Job.t -> unit
  ; get :
      Agent_protocol.Job.launch_owner
      -> Agent_protocol.Id.Job.t
      -> (Agent_protocol.Job.t, Agent_protocol.Error.t) result
  ; cancel :
      Agent_protocol.Job.launch_owner
      -> Agent_protocol.Id.Job.t
      -> (unit, Agent_protocol.Error.t) result
  }

type t
type scope

(** Payload-free status/completion projection. The host may apply additional
    disclosure policy before returning it to a script. *)
val view : Agent_protocol.Job.t -> (Jsonaf.t, Agent_protocol.Error.t) result

val create
  :  env:Eio_unix.Stdenv.base
  -> policy:Chat_response.One_off_request.policy
  -> current_capabilities:(unit -> Chat_response.Tool_capability.t)
  -> host:host
  -> t

(** Bind to a host-verified caller selection and active owner. No raw model input
    may choose owner or selected. Source compilation captures a subset of this
    ceiling and rechecks it after the domain wait. Scope handles expire on return.
    Failure/exception releases all starts; success retains selected reservations
    for the actor's enclosing outcome/checkpoint commit. *)
val with_scope
  :  t
  -> owner:Agent_protocol.Job.launch_owner
  -> selected:Chat_response.Tool_capability.t
  -> error:(string -> 'error)
  -> (scope -> ('a, 'error) result)
  -> ('a, 'error) result

val install
  :  ?control:Chatml.Chatml_lang.execution_control
  -> scope
  -> Chatml_host_runtime.runtime_config
  -> Chatml_host_runtime.runtime_config

(** Validate/select exact surviving starts after all output/disclosure checks.
    Returns ordinary effects for the moderator's normal transactional decoder. *)
val select
  :  scope
  -> Chatml.Chatml_lang.eff list
  -> (Chatml.Chatml_lang.eff list, string) result

val validate_work : scope -> Agent_protocol.Invocation.work -> (unit, string) result
