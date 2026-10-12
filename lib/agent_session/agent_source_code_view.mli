open Core

(** Immutable selected declaration associations for retained native Agent functions.
    No function/capability is registered or changed, and no source loan is opened.
    The selected runtime switch owns this code view, not the native resources. *)
type t

module Context : sig
  type t

  val revision : t -> Prompt_revision.t
  val reference : t -> Prompt.Chat_markdown.Agent_reference.t
end

(** Both revisions are genuinely admitted, unaugmented parser owners. The caller
    has validated their complete native footprint and keeps the original native
    resources alive. Match actual emitted Tool/Persistent_tool occurrences to the
    original registry's exact native function values, checking declaration kind,
    portable queued member and unchanged tool interface; source directories and
    JSON/name descriptors never supply source authority. The capability lookup is
    only the owning registry's declaration association, not a new registration.

    Bound declaration/registration counts before retention; compare input-sized
    fields under the borrowed whole-helper checkpoint. The switch terminally
    revokes lookups. Exceptions/cancellation preserve their original identity. *)
val create
  :  original:Prompt_revision.t
  -> selected:Prompt_revision.t
  -> native:Chat_response.Agent_runtime.t
  -> sw:Eio.Switch.t
  -> max_work_steps:int
  -> checkpoint:(unit -> unit)
  -> (t, Agent_protocol.Error.t) Result.t

(** Actual dispatch checks current native-service/ceiling/actor ownership before
    this lookup. Only the SAME original function and defining revision can select
    a context. A runner already bound to the selected revision receives None,
    preventing recursive redirection; unrelated nested functions receive None.
    An associated original function presented with a foreign defining revision
    rejects; it cannot fall back to its original source. Closed views fail even
    for a nonmatching function. No authorization or source
    acquisition occurs here. *)
val find
  :  t
  -> defining_revision:Prompt_revision.t
  -> implementation:Ochat_function.t
  -> (Context.t option, Agent_protocol.Error.t) Result.t

(** Source preparation before queue staging has no native invocation yet. Its
    existing job/registration owner validates the binding and request first.
    This exact-function projection supplies the immutable context to that owner;
    it cannot manufacture a Delegation key or source authority. *)
val find_for_preparation
  :  t
  -> implementation:Ochat_function.t
  -> (Context.t option, Agent_protocol.Error.t) Result.t
