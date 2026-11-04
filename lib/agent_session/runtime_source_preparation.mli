open! Core

(** Immutable source selection before target runtime construction. Ordinary
    captured sources supply no live tool ABI; generated selections retain their
    actual parent's already admitted bindings under the host's resource lease.
    Neither branch is an initialized runtime, invocation capability or permission
    to execute. *)
type t

module Selection : sig
  (** Distinct actual source owners. A generated definition retains its admitted
      inherited bindings and compiled programs; it is never an ordinary revision. *)
  type t =
    | Authored of Prompt_revision.t
    | Captured of Captured_recurrence_source.t
    | Authored_child of
        { source : Prompt_child_source.t
        ; reference : Agent_store.Delegation_store.Reference.t
        }
    | Generated of Generated_definition.t
end

module Moderator : sig
  (** Legacy identity remains distinct from the versioned invocation contract.
      No_moderator is explicit; it cannot authorize an authored Run startup. *)
  type t =
    | No_moderator
    | Legacy of Agent_protocol.Invocation.observer
    | Extension of Agent_protocol.Invocation.observer
  [@@deriving equal, sexp_of]
end

(** Validate the actual revision's retained parser, invocation role and complete
    selected program custody under the existing bounded startup policy. Source
    identity uses the same single-file/linked-program recipe as the live Registry.
    No original source lookup, native/MCP setup, tool grant, Manager construction,
    initial content conversion, history allocation or initializer occurs.

    The caller owns source admission and artifact/tree verification. This does
    not establish a Session_actor or Runtime_owner binding. The actual target
    preparation must bind this value before held Run admission. Full live tool
    admission and compilation remain required before any initializer, after the
    separate execution barrier. Cancellation and checkpoint exceptions propagate.
    No borrowed startup context is retained. *)
val prepare
  :  Prompt_revision.t
  -> script_preparation:Runtime_script_preparation.t option
  -> invocation:Runtime_script_preparation.Invocation.t
  -> env:Eio_unix.Stdenv.base
  -> checkpoint:(unit -> unit)
  -> (t, Agent_protocol.Error.t) result

(** Retain the actual captured child admission while selecting its ordinary
    programs. The original artifact's defining metadata is preserved, not
    rewritten to the target's absent catalog identity. *)
val prepare_captured
  :  Captured_recurrence_source.t
  -> script_preparation:Runtime_script_preparation.t option
  -> invocation:Runtime_script_preparation.Invocation.t
  -> env:Eio_unix.Stdenv.base
  -> checkpoint:(unit -> unit)
  -> (t, Agent_protocol.Error.t) result

(** Retain a fully restored authored child and exact ledger locator as data.
    Checks the recorded authored family and selected artifact reference. The host
    must FIRST authenticate the actual wrapper/private resources and retain its
    parent lease; this method grants none of that authority. *)
val prepare_authored_child
  :  Prompt_child_source.t
  -> record:Agent_store.Delegation_store.record
  -> script_preparation:Runtime_script_preparation.t option
  -> invocation:Runtime_script_preparation.Invocation.t
  -> env:Eio_unix.Stdenv.base
  -> checkpoint:(unit -> unit)
  -> (t, Agent_protocol.Error.t) result

(** Bind the existing generated owner's validated compiled definition without
    constructing a Manager, initializing scripts or resolving new capabilities.
    The host must verify the recorded artifact and retain the actual parent
    resource lease through every consumer. This value grants no execution or
    resource authority and cannot replace that lease. *)
val prepare_generated
  :  Generated_definition.t
  -> invocation:Runtime_script_preparation.Invocation.t
  -> (t, Agent_protocol.Error.t) result

val selection : t -> Selection.t
val artifact : t -> Agent_store.Prompt_artifact_store.Artifact.t

(** Authored selections retain the actual defining policy even when legacy
    artifact metadata is absent. Generated selections keep their artifact's
    actual definition absence. *)
val definition_id : t -> Agent_protocol.Id.Prompt_definition.t option

val invocation : t -> Runtime_script_preparation.Invocation.t
val moderator : t -> Moderator.t

(** Ordinary authored captured-program custody; no compiler deadline is retained.
    None includes validated ordinary single-file absence. Generated compiled
    programs and their inherited bindings remain in [selection]'s Generated
    branch rather than being converted to ordinary captured custody. *)
val programs : t -> Chatml_prompt_programs.Restored.t option

(** Pure exact target/family comparison. Every child selection checks its actual
    immutable child association, even when another session shares its artifact.
    Ordinary missing-definition rules remain strict. No callbacks, current
    operator/delegation permission, runtime admission or execution proof here. *)
val validate_target : t -> Session_state.t -> (unit, Agent_protocol.Error.t) result
