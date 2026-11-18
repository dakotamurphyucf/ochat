open! Core

(** Immutable source selection before target runtime construction. Ordinary
    captured sources supply no live tool ABI; generated selections retain their
    actual parent's already admitted bindings under the host's resource lease;
    inert generated artifact selections retain DATA only, never that ABI.
    Neither branch is an initialized runtime, invocation capability or permission
    to execute. *)
type t

module Selection : sig
  (** Distinct actual source owners. A generated definition retains its admitted
      inherited bindings and compiled programs. Generated_artifact instead
      retains only validated immutable DATA; it is neither executable admission
      nor an ordinary revision. *)
  type t =
    | Authored of Prompt_revision.t
    | Captured of Captured_recurrence_source.t
    | Authored_child of
        { source : Prompt_child_source.t
        ; reference : Agent_store.Delegation_store.Reference.t
        }
    | Generated of Generated_definition.t
    | Generated_artifact of Generated_definition.Source.t
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

(** Alias for the enclosing immutable Source result returned by delayed binding. *)
type source = t

module Revision_input : sig
  (** Actual immutable source family and target association DATA. The authored
      child constructor requires its real installed source and admission record;
      none of these inputs supplies current private/Goal/native permission. *)
  type t

  val authored
    :  Prompt_revision.t
    -> invocation:Runtime_script_preparation.Invocation.t
    -> t

  (** Same root/origin validation as [prepare_captured]. Captured Authored_tool
      preserves its genuine restricted Delegated_ui role and actual origin. *)
  val captured
    :  Captured_recurrence_source.t
    -> invocation:Runtime_script_preparation.Invocation.t
    -> (t, Agent_protocol.Error.t) result

  (** Preserve the existing authored admission/artifact/reference checks. Stored
      Source invocation is the actual target Ui/Headless root; preparation uses
      the existing authentic Delegated_ui specialist program contract. *)
  val authored_child
    :  Prompt_child_source.t
    -> record:Agent_store.Delegation_store.record
    -> invocation:Runtime_script_preparation.Invocation.t
    -> (t, Agent_protocol.Error.t) result

  val revision : t -> Prompt_revision.t
  val binding_invocation : t -> Runtime_script_preparation.Invocation.t
  val preparation_invocation : t -> Runtime_script_preparation.Invocation.t
end

module Prepared_revision : sig
  (** Actual immutable revision/artifact, program role, selected moderator and
      restored programs, before a future target Reference exists. No startup,
      checkpoint, callback, resource/native owner or permission is retained. *)
  type t

  (** Capture inside the SAME original with_revision_plan callback using its
      actual revision/role, limits, validated plan/programs and borrowed startup.
      Reuses bounded moderator selection; no parser, restore, compile or native
      setup. None/None must be the preparation's genuine checked result and is
      never by itself No_moderator. Expected failures remain typed; original
      checkpoint exceptions/cancellation propagate. Neither context escapes. *)
  val capture
    :  Prompt_revision.t
    -> invocation:Runtime_script_preparation.Invocation.t
    -> startup:Chat_response.Startup_compilation.t
    -> compilation_limits:Chatml_compilation.limits
    -> plan:Chat_response.Script_contract_plan.t option
    -> programs:Chatml_prompt_programs.Restored.t option
    -> checkpoint:(unit -> unit)
    -> (t, Agent_protocol.Error.t) result

  (** Read the exact moderator DATA selected by [capture] for its actual
      revision/program role. This constant-time projection performs no IO or
      callback and supplies no target association, wrapper/private registry,
      permission or full Source. Metadata callers separately verify the real
      ledger, defining source/publication and current target tuple. *)
  val moderator : t -> Moderator.t

  (** Build full immutable Source only with its real source-family/target input.
      Require the actual preparation role and exact publication, authored bytes
      and program/content metadata/payloads. Only ordinary authored child may
      apply Artifact.with_identity to its real admission revision/time; original
      admitted publication digest and full data must then match. No bare ID or
      same_content shortcut, parsing, restoration, program relabel or setter.
      Current private/source/Goal authority remains with the caller. *)
  val bind : t -> input:Revision_input.t -> (source, Agent_protocol.Error.t) result
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

(** Retain the actual captured child admission and original defining metadata.
    The supplied invocation must match the origin-derived child root family.
    Artifact sources retain that root invocation; captured authored specialists
    select the existing restricted Delegated_ui role with the child Standard ABI.
    The defining parent's root and tool ABI remain unchanged provenance. Exact
    retained compiled-role custody must match this child contract; it is not
    recompiled into another ABI. [validate_target] checks the same derived role;
    unrelated source kinds cannot use it to widen invocation matching.

    The Factory must authenticate the actual wrapper/private resources and retain
    the genuine parent resource loan through consumers. The source discriminator
    is immutable data and grants none of that authority. *)
val prepare_captured
  :  Captured_recurrence_source.t
  -> script_preparation:Runtime_script_preparation.t option
  -> invocation:Runtime_script_preparation.Invocation.t
  -> env:Eio_unix.Stdenv.base
  -> checkpoint:(unit -> unit)
  -> (t, Agent_protocol.Error.t) result

(** Retain a fully restored authored child and exact ledger locator as data.
    Checks the recorded authored family and selected artifact reference. Source
    binding preserves the actual target root invocation; program preparation uses
    the same Delegated_ui contract as original and selected specialist execution.
    The host must FIRST authenticate the actual wrapper/private resources and retain its
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

(** Immutable generated artifact DATA with no executable bindings or Program.
    Full generated restoration and current parent admission remain mandatory
    during the separately admitted construction. [validate_target] checks its
    exact child/delegation locator, revision and root invocation. *)
val prepare_generated_artifact
  :  Generated_definition.Source.t
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
    Captured authored sources must match the origin-derived child Ui family
    and restricted delegated role, independently of the parent's defining role.
    Ordinary missing-definition rules remain strict. No callbacks, current
    operator/delegation permission, runtime admission or execution proof here. *)
val validate_target : t -> Session_state.t -> (unit, Agent_protocol.Error.t) result
