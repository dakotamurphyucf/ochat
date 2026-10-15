open! Core

(** Interpreter for one already claimed developer Script job. The enclosing
    [Developer_background_job_runner] derives the saved request from the actual
    Store Claim, calls [Background_request.prepare] under its current selected
    registry, and holds [Developer_script_job_service.with_claimed_worker]
    through the parent/selected-child Store handoff. This private module owns
    no Claim, native borrow, writer, result publisher, capacity or scheduler. *)

type t

val completion : t -> Agent_protocol.Completion.t
val local_effects : t -> Chatml.Chatml_lang.eff list

(** Build an expiry completion without compiling or evaluating the Script.
    It still admits the entire encoded Completion against the saved output
    policy, so an undersized envelope is a typed resource failure. *)
val expired
  :  policy:Chat_response.One_off_request.policy
  -> (t, Agent_protocol.Error.t) result

(** Run the exact [One_off_v1] program produced from the claimed request.
    [scope] is the live lexical scope issued for that same Claim; it installs
    Job operations but cannot add Model.spawn to this compiler surface.
    [call_tool] is supplied by the enclosing physically bound runner. It must
    resolve a name only in [One_off_script.capabilities prepared], construct an
    actual developer Host_invocation with this Claim as parent, and call its
    selected [Developer_native_executor.run_job_child] with the same live
    [active] proof. This callback is an effect, not evidence supplied by the
    stored request. A native Pending(Job) returns its genuine acknowledgement
    to Tool.call; unsupported work and failed physical admission reject.

    The enclosing runner starts the host budget before saved-request
    preparation/compilation. One nested bounded [Chatml_execution.with_control]
    scope inherits that budget and covers input import, evaluation, selected
    Tool.call effects, output projection and the encoded result. [deadline] is
    the stored request deadline, already checked against its host policy; this
    call samples [now] again and narrows remaining wall time. No native
    invocation is fabricated for the bare Script. A normal
    language failure returns a bounded [Completion.Failed]; expiry returns
    [Completion.Expired]; both have no selected local effects. A successful
    main returns [Completion.Succeeded] and the exact surviving local effects
    for the enclosing lexical owner to split and seal. Task.catch rolls back
    discarded local effects; external managed effects have independent custody.
    An effect-admission [Error] exits the interpreter immediately: Task.catch
    cannot convert it into Tool.error success or allow a later native effect.
    Eio cancellation, unexpected callback exceptions and their original
    backtraces propagate. No callback or interpreter execution repeats after
    an uncertain Store save. *)
val run
  :  env:Eio_unix.Stdenv.base
  -> scope:Developer_script_job_service.Scope.t
  -> prepared:Chat_response.One_off_script.t
  -> input:Jsonaf.t
  -> policy:Chat_response.One_off_request.policy
  -> deadline:Agent_protocol.Timestamp.t
  -> now:(unit -> Agent_protocol.Timestamp.t)
  -> call_tool:
       (name:string
        -> args:Jsonaf.t
        -> (Agent_protocol.Invocation.outcome, Agent_protocol.Error.t) result)
  -> (t, Agent_protocol.Error.t) result
