open! Core

(** One selected child's document/image conversion. The trusted child
    Target first authenticates the restored revision against its physical
    Source_binding and Root; only then may it bind a
    [Prompt_source_owner.Scope.Content_revision] and construct this projector.
    A parser-owned Basic occurrence, not a model URL or Ctx directory, selects
    its declaring source. Declaration and invocation projectors share one
    physical Budget seeded with already captured content costs. *)
type t

module Limits : sig
  type t

  val create
    :  max_input_bytes:int
    -> max_output_bytes:int
    -> max_work_steps:int
    -> max_nodes:int
    -> max_depth:int
    -> t Or_error.t

  (** The configured capture ceiling also bounds selected conversion. When
      capture is disabled, literal selected content still receives a fixed
      finite ceiling; no source read is enabled by that default. *)
  val for_host : Chatmd_prompt_content.Limits.t option -> t Or_error.t
end

module Budget : sig
  type t

  (** Create once after authentic declaration/caller selection. These at most
      two exact source-bound revisions share one budget; their captured/builtin
      accounting is debited before dynamic reads. A physically identical
      caller revision is charged once. A resource failure rejects before any
      new source I/O. *)
  val create
    :  limits:Limits.t
    -> declaration:Agent_session.Prompt_revision.t
    -> caller:Agent_session.Prompt_revision.t option
    -> (t, Agent_protocol.Error.t) result

  (** Admit one further authenticated nested child's declaring and actual
      caller revisions into this SAME budget before conversion. Debit each new
      physical retained revision's captured accounting exactly once; a caller
      already admitted by an ancestor is not charged again. This is resource
      accounting, not source authority: the selected Target and source ports
      are checked separately. The short budget gate covers membership/debit
      only and is released before any nested Agent callback. *)
  val admit_revisions
    :  t
    -> declaration:Agent_session.Prompt_revision.t
    -> caller:Agent_session.Prompt_revision.t
    -> (unit, Agent_protocol.Error.t) result

  module Executable_cost : sig
    type t

    (** Actual nonnegative ChatMD authored root/dependency bytes and charged
        outer selection/capture checkpoints observed by the bounded lower
        operation. The budget validates and debits these under its own gate.
        ChatML program/project acquisition retains its existing independent
        max_total_input_bytes, project_work_steps and compiler ceilings; it is
        neither guessed from a selected member nor charged a second time as
        ChatMD bytes in this ledger. Captured document accounting is debited
        separately by [create]/[admit_revisions]. *)
    val create : input_bytes:int -> work_steps:int -> t Or_error.t
  end

  (** Use this SAME child ledger for one Model executable-member selection.
      Serialize allowance snapshot, ChatMD source acquisition and debit so parallel
      Model.spawn children cannot both spend it. [on_acquire] is called once
      immediately before any native/network read or partially allocating
      capture. A proven denial before it leaves the budget usable; a later
      error/cancellation conservatively marks only this child budget uncertain
      or retains the spent reservation. No Scope, gate or mutable allowance
      escapes to provider inference, nested Agent callbacks or job joins.
      [f] must return observed lower-source costs, never a caller guess. *)
  val with_executable_source_read
    :  t
    -> f:
         (max_input_bytes:int
          -> max_work_steps:int
          -> on_acquire:(unit -> unit)
          -> ('a * Executable_cost.t, Agent_protocol.Error.t) result)
    -> ('a, Agent_protocol.Error.t) result
end

type source =
  | Captured_only
  | Selected of Agent_session.Captured_agent_runner.Source_binding.Content_source.t

(** [remote=None] denies remote image/document reads before network I/O. The
    remote port is constructed only by explicit trusted daemon configuration.
    [check_current] rechecks the exact selected Target, parent/Root and source
    borrow after every yielding phase; it is not itself source authority. *)
val create
  :  budget:Budget.t
  -> revision:Agent_session.Prompt_revision.t
  -> source:source
  -> side:Agent_session.Captured_agent_runner.Source_binding.Content_source.side
  -> remote:Developer_child_remote_content.t option
  -> check_current:(unit -> (unit, Agent_protocol.Error.t) result)
  -> checkpoint:(unit -> unit)
  -> (t, Agent_protocol.Error.t) result

(** Reuses the one Chat_response.Converter role/string traversal for the
    selected message/reasoning subset. Tool-call history is not admitted here.
    Captured and builtin documents were already materialized by the selected
    revision; only parser-owned dynamic/local/remote occurrences are read.
    Inline image string rendering retains its original spelling without a
    read. [agent] executes only the exact parser-owned occurrence through the
    selected child's declaring or caller port. The callback's descendant
    Target and joined owner are established by the upper child runner, not by
    Converter. The source Scope is closed before this callback, so synchronous
    nested selection never enters a recursively held read gate. Failure and
    cancellation cannot reset the aggregate budget or retry a source read. *)
val to_items
  :  t
  -> ctx:Eio_unix.Stdenv.base Chat_response.Ctx.t
  -> agent:(Prompt.Chat_markdown.agent_content -> (string, Agent_protocol.Error.t) result)
  -> Prompt.Chat_markdown.top_level_elements list
  -> (Openai.Responses.Item.t list, Agent_protocol.Error.t) result
