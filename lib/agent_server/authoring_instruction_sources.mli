open! Core

(** Borrow the installation's existing source owner. This opens no root, owns no
    store and grants no project Build or execution. The configured policy must
    already be physically bound to that source owner's Project_source_host.
    [is_current] is the installation's nonyielding close check.

    Every Use acquires genuine current Root selections for the original actor;
    root labels are administrative configured-spelling provenance, not durable
    filesystem identity. Changed spelling cannot silently authorize old captures.
    Each operation has at most 64 results, 256 KiB content and 131072 work polls.
    Checkpoint/cancellation exceptions retain their identity. *)
val create
  :  Authoring_source_host.t
  -> configured:Authoring_configured_project_policy.t
  -> is_current:(unit -> bool)
  -> Agent_session.Instruction_source_port.t
