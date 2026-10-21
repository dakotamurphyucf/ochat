open! Core
module Sources = Authoring_source_host
module Context = Agent_protocol.Authoring.Context
module Path_candidate = Chatmd_authoring_assistance.Path_candidate
module Cursor = Language_highlighting.Highlight_snapshot.Cursor
module Error = Authoring_context_service_core.Error

(** Optional name-only path candidates for a CURRENT host-member document.
    The genuine Document_context owns the source root and checks original plus
    request actors during bounded nonrecursive listing; [view] supplies only its
    connection-local root label for wire evidence. The selected cursor must be
    an actual Authored ChatMD attribute value for [src] or [path]. Every
    candidate's spelling is checked by the existing pure confined
    Source_loader.resolve_member against the exact listed portable member.
    No entry is read or claimed to be a regular file; supplied documents and
    other cursor states return []. More native entries than [max_paths] or
    exhausted projection work returns Resource_limit, never a false Complete
    partial list. The owning service rechecks context/root currentness after
    this yielding call and before response publication. *)
val host_document
  :  owner:Sources.Document_context.t
  -> view:Context.t
  -> source:Chatml_source.t
  -> cursor:Cursor.t
  -> actor:Operator_authorization.t
  -> max_paths:int
  -> max_projection_work_steps:int
  -> checkpoint:(unit -> unit)
  -> (Path_candidate.t list, Error.t) result
