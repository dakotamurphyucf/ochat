open! Core

(** Private admitted selection for one exact Saved, Content, Model or Tool target under an
    already entered developer native callback. The owning Host obtains
    [prepared] from the same physical Live package and Target; this module
    admits no arbitrary registry, owner coordinates or copied artifact as
    authority. It never widens the parent Root selection. *)
type t

module Admission : sig
  type t

  (** The child-only capability set paired with one freshly checked source,
      Root and Live witness. The exact saved-parent transition receives the
      admitted child selection; a nested child receives only its own physically
      borrowed ceiling intersected with that selection. A later effect must refresh again; the witness
      cannot be retained as a mutable grant across sibling attempts. *)
  val capabilities : t -> Chat_response.Tool_capability.t

  (** The original child-source registry for checking the managed handler's
      own binding. This is distinct from [capabilities], which is narrowed by
      the entered managed program's dependency scope. It does not authorize a
      nested Tool.call; callers must use [capabilities] for those effects. *)
  val source_capabilities : t -> Chat_response.Tool_capability.t

  (** Final nonyielding check after the attempt's last wait and immediately
      before effect entry. It rejects a changed Root effect revision, actor,
      selected Target, source binding or child scope. *)
  val check_now : t -> (unit, Agent_protocol.Error.t) result

  (** Check this exact physical child borrow, Target and Prepared admission
      without yielding, then return the Root Current stored in that SAME
      admission. The native executor compares capabilities and Root selection
      from one refreshed attempt; this does not mint or rebase authority. *)
  val checked_root_now
    :  t
    -> (Developer_root_owner.Current.t, Agent_protocol.Error.t) result
end

val create
  :  root:Developer_root_owner.run
  -> target:Agent_session.Captured_agent_runner.Claimed_developer.Selected.Target.t
  -> prepared:Developer_live_resources.Child_prepared.t
  -> parent:Agent_session.Native_tool_invocation.selected_borrowed
  -> (t, Agent_protocol.Error.t) result

(** Admit one parser-owned Content child beneath the exact logical [parent]
    selection, using the current ACTUAL selected native borrow as physical
    issuer/active. The Content target and prepared resources must match that
    parent and the same Root. The child's own source-prepared registry may have
    NEW managed/native declarations admitted by its captured code and manifest;
    those are not required to occur in the parent's registry. The parent's
    current selection remains a live ancestry proof, and no parent binding is
    inherited except through its existing exact selection. As in [create], an
    actually entered native descendant narrows against its physical borrowed
    ceiling before effect entry. Content selection alone does not
    mint a new native invocation or promote the parent's source-child issuer
    to the physical active issuer. The existing source-child transition uses
    the actual borrow/active only when a tool later dispatches. *)
val create_content
  :  root:Developer_root_owner.run
  -> parent:t
  -> target:Agent_session.Captured_agent_runner.Selected_content_target.t
  -> prepared:Developer_live_resources.Child_prepared.t
  -> borrowed:Agent_session.Native_tool_invocation.selected_borrowed
  -> (t, Agent_protocol.Error.t) result

(** Select one Model member under the exact immediate Saved, Content, Model or Tool
    child and the ACTUAL entered native borrow. The Model target is minted by
    the checked member/Port bracket, not a parser Agent occurrence. Its own
    authenticated source may introduce new source-owned declarations, while
    inherited native authority still follows the physical borrowed ceiling.
    This reuses [create_content]'s restricted Source_child issuer and final
    Root/parent checks; it does not create an invocation by selecting a path. *)
val create_model
  :  root:Developer_root_owner.run
  -> parent:t
  -> target:Agent_session.Captured_agent_runner.Selected_model_target.t
  -> prepared:Developer_live_resources.Child_prepared.t
  -> borrowed:Agent_session.Native_tool_invocation.selected_borrowed
  -> (t, Agent_protocol.Error.t) result

(** Select the lower parser-owned ordinary Agent Tool target beneath its exact
    immediate child. It uses the actual current selected Native borrow and
    intersects the same Root/parent ceiling; it never casts a Worker Claim or
    widens to Root's registry. *)
val create_tool
  :  root:Developer_root_owner.run
  -> parent:t
  -> target:Agent_session.Captured_agent_runner.Selected_tool_target.t
  -> prepared:Developer_live_resources.Child_prepared.t
  -> borrowed:Agent_session.Native_tool_invocation.selected_borrowed
  -> (t, Agent_protocol.Error.t) result

(** Exact restricted issuer and first-entry parent derived at [create],
    [create_content], [create_model] or [create_tool]. The caller may use them only together in the actual
    selected native callback. Logical nested Content selection can retain the
    same physical callback; later tool entries use the new issuer's own active. *)
val issuer : t -> Agent_session.Developer_invocation_owner.Selected.t

(** Comparison only; a managed child cannot pair this selection with a
    different physical Root. *)
val issued_by_root : t -> Developer_root_owner.run -> bool

(** Capture only from this checked selected child's retained Live preparation.
    [live] must be the exact same physical package; it grants no selection by
    itself. Refresh before and after yielding projection, including the actual
    native borrow, immediate target and Root/actor source proof. *)
val capture_issuer_source
  :  t
  -> live:Developer_live_resources.live
  -> checkpoint:(unit -> unit)
  -> (Chat_response.Background_request.Issuer_source.t, Agent_protocol.Error.t) result

(** Prepare a source artifact from this child's OWN installed native runtime.
    The supplied [live] must physically own its Prepared resources. Refresh
    the exact Target, Root and source before and after the potentially yielding
    preparation; never retry a denied child binding against the root runtime.
    The caller also checks the binding against its lexical Job scope. *)
val prepare_agent_source
  :  t
  -> live:Developer_live_resources.live
  -> binding:Chat_response.Tool_capability.binding
  -> launch:Chat_response.Source_launch.t
  -> request:Chat_response.Background_request.t
  -> max_registrations:int
  -> checkpoint:(unit -> unit)
  -> ( Chat_response.Background_request.Agent_artifact.t option
       , Agent_protocol.Error.t )
       result

val first_parent : t -> Agent_session.Developer_invocation_owner.Selected.parent

(** The first child tool entry must still be under the exact saved parent
    borrow. Return that physical borrow only after comparing it with the
    current selected scope; its original issuer supplies the inherited Job
    ancestor for the Source_child transition. *)
val first_borrowed
  :  t
  -> ( Agent_session.Native_tool_invocation.selected_borrowed
       , Agent_protocol.Error.t )
       result

(** The managed definition is obtained from [prepared], never separately
    supplied alongside a child registry. *)
val managed_definition : t -> Chat_response.Managed_tool_registry.t

(** Borrow the actual selected scope and classify it by physical equality to
    this selection's retained entering borrow. The direct branch uses strict
    Saved, Content, Model or Tool target/Root proof; a different current borrow must prove
    an entered native descendant. Logical Content recursion can share one
    entering borrow while each selection still narrows capabilities. Missing,
    stale or foreign borrows reject without fallback.
    Refresh may yield. Every caller must use the returned [Admission] for one
    attempt and repeat this operation after any further wait. *)
val refresh : t -> (Admission.t, Agent_protocol.Error.t) result

(** Recheck the physical selected scope, Root and saved source, then project the
    child's original registry. This is only a currentness/comparison input for
    an already admitted invocation. Nested effects must still intersect their
    actual borrowed ceiling. *)
val source_current : t -> (Chat_response.Tool_capability.t, Agent_protocol.Error.t) result
