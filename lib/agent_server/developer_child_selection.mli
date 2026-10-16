open! Core

(** Private admitted selection for one exact saved-Agent target under an
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
end

val create
  :  root:Developer_root_owner.run
  -> target:Agent_session.Captured_agent_runner.Claimed_developer.Selected.Target.t
  -> prepared:Developer_live_resources.Child_prepared.t
  -> parent:Agent_session.Native_tool_invocation.selected_borrowed
  -> (t, Agent_protocol.Error.t) result

(** Exact restricted issuer and first-entry parent derived at [create]. The
    caller may use them only together in the physical saved callback; ordinary
    nested child entries use this issuer's own newly entered active. *)
val issuer : t -> Agent_session.Developer_invocation_owner.Selected.t

(** Comparison only; a managed child cannot pair this selection with a
    different physical Root. *)
val issued_by_root : t -> Developer_root_owner.run -> bool

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
    the saved parent borrow. The saved parent branch uses strict direct
    Target/Root proof; a different current borrow must prove an entered child
    descendant. Missing, stale or foreign borrows reject without fallback.
    Refresh may yield. Every caller must use the returned [Admission] for one
    attempt and repeat this operation after any further wait. *)
val refresh : t -> (Admission.t, Agent_protocol.Error.t) result

(** Recheck the physical selected scope, Root and saved source, then project the
    child's original registry. This is only a currentness/comparison input for
    an already admitted invocation. Nested effects must still intersect their
    actual borrowed ceiling. *)
val source_current : t -> (Chat_response.Tool_capability.t, Agent_protocol.Error.t) result
